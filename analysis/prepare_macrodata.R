#
# Prepare several country-specific macrodata sources.
#
# March 2021, Markus Konrad <markus.konrad@wzb.eu>
#

library(dplyr)
library(rvest)
library(stringr)
library(readxl)
library(countrycode)

source('dataprep.R')

LAST_CONTRIBS_DATE = '2020-08-02'

countries <- read.csv(file.path('..', 'collection', 'data', 'countries.csv'), stringsAsFactors = FALSE)$country_code
countrycodes2names <- read.csv(file.path('aggregdata', 'countrycodes2names.csv'), stringsAsFactors = FALSE) %>% filter(country_code %in% countries)

##### StackOverflow data #####

so_yearly_cols <- list(
  '2017' = list(
    'age' = 'NA_col',
    'remote' = 'HomeRemote',
    'dependents' = 'NA_col',
    'hobby' = 'ProgramHobby',
    'professional' = 'Professional'
  ),
  '2018' = list(
    #'age' = 'Age',                 # coded as ranges like "18 - 24 years old"
    'age' = 'NA_col',
    'remote' = 'NA_col',
    'dependents' = 'Dependents',
    'hobby' = 'Hobby',
    'professional' = 'NA_col'
  ),
  '2019' = list(
    'age' = 'Age',
    'remote' = 'WorkRemote',
    'dependents' = 'Dependents',
    'hobby' = 'Hobbyist',
    'professional' = 'MainBranch'
  ),
  '2020' = list(
    'age' = 'Age',
    'remote' = 'NA_col',
    'dependents' = 'NA_col',
    'hobby' = 'Hobbyist',
    'professional' = 'MainBranch'
  )
)

sosurveys_years <- list()
for(year in names(so_yearly_cols)) {
  print(year)
  so_cols <- so_yearly_cols[[year]]
  sosurvey_raw <- read.csv(sprintf('macrodata/stackoverflow-survey/%s/survey_results_public.csv', year),
                           colClasses = 'character',
                           stringsAsFactors = FALSE)
  #sort(unique(sosurvey_raw$Country))

  sosurvey <- mutate(sosurvey_raw, Country = case_when(
    Country == 'Russian Federation' ~ 'Russia',
    Country == 'Czech Republic' ~ 'Czechia',
    Country == 'Viet Nam' ~ 'Vietnam',
    TRUE ~ Country
  ))

  stopifnot(sum(!(countrycodes2names$country_name %in% unique(sosurvey$Country))) == 0)
  #countrycodes2names$country_name[!(countrycodes2names$country_name %in% unique(sosurvey_raw$Country))]

  sosurveys_years[[year]] <- inner_join(sosurvey, countrycodes2names, by = c('Country' = 'country_name')) %>%
    mutate(year = as.integer(year), NA_col = NA_character_) %>%
    select(year, country_code, gender = Gender, age = so_cols$age, remote = so_cols$remote, dependents = so_cols$dependents, hobby = so_cols$hobby, professional = so_cols$professional) %>%
    mutate(gender = case_when(gender %in% c('Man', 'Male') ~ 'male',        # to match with GitHub contributions data
                              gender %in% c('Woman', 'Female') ~ 'female',
                              TRUE ~ NA_character_),
           mostly_remote = ifelse(is.na(remote), NA,   # somehow NA's don't propagate here
                                  remote %in% c('About half the time',
                                                "All or almost all the time (I'm full-time remote)",
                                                'More than half, but not all, the time')),
           dependents = dependents == 'Yes',
           professional = ifelse(is.na(professional), NA, professional %in% c("I am a developer by profession", "Professional developer")),
           hobby = case_when(startsWith(hobby, 'Yes') ~ TRUE,
                             startsWith(hobby, 'No') ~ FALSE,
                             TRUE ~ NA)) %>%
    filter(!is.na(gender)) %>%
    select(-remote)
}

sosurvey_full <- bind_rows(sosurveys_years)
summary(sosurvey_full)

filter(sosurvey_full, is.na(as.numeric(age))) %>% distinct(age)

# clean age (convert age ranges to numeric vales – only if age in 2018 is included!)
sosurvey_full <- mutate(sosurvey_full, age = case_when(
    age == 'Under 18 years old' ~ 17,
    str_ends(age, 'years old') ~ apply(str_match(age, '^(\\d+) - (\\d+) years old$'), 1, function(row) mean(as.integer(row[2:3]))),
    age == '65 years or older' ~ 65,
    TRUE ~ as.numeric(age)
  )) %>%
  filter(is.na(age) | (age >= 14 & age <= 99))

summary(sosurvey_full)

# simple checks

hist(sosurvey_full$age, breaks = 50)   # right-skewed
table(sosurvey_full$gender)
table(sosurvey_full$mostly_remote)
table(sosurvey_full$dependents)
table(sosurvey_full$hobby)
table(sosurvey_full$professional)

# aggregate

soaggreg <- group_by(sosurvey_full, country_code, gender) %>%
  summarise(
    dev_samplesize = n(),
    dev_age_mean = mean(age, na.rm = TRUE),
    dev_age_median = median(age, na.rm = TRUE),
    dev_age_sd = sd(age, na.rm = TRUE),
    dev_age_n = sum(!is.na(age)),             # non-NA for specific variable
    dev_mostly_remote_share = sum(mostly_remote, na.rm = TRUE) / n(),
    dev_mostly_remote_n = sum(!is.na(mostly_remote)),
    dev_dependents_share = sum(dependents, na.rm = TRUE) / n(),
    dev_dependents_n = sum(!is.na(dependents)),
    dev_hobby_share =  sum(hobby, na.rm = TRUE) / n(),
    dev_hobby_n = sum(!is.na(hobby)),
    dev_prof_share =  sum(professional, na.rm = TRUE) / n(),
    dev_prof_n = sum(!is.na(professional)),
  )    
View(soaggreg)

filter(soaggreg, dev_age_n < 20 | dev_mostly_remote_n < 20 | dev_dependents_n < 20 | dev_hobby_n < 20 | dev_prof_n < 20)

write.csv(soaggreg, 'macrodata/stackoverflow_survey_2017-2020_aggreg.csv', row.names = FALSE)


##### COVID19 and gov. response data #####

# see https://covid19datahub.io/articles/doc/data.html
coviddata <- load_coviddata(countries, '2020-01-01', '2020-08-09',
                            cachefile = 'cache/coviddata.RDS', min_stay_home_restrictions_level = NULL) %>%
  ungroup()   # the data comes grouped by "id"

stopifnot(sum(!(countries %in% unique(coviddata$iso_alpha_2))) == 0)

covid_relevant <- select(coviddata, country_code = iso_alpha_2, date, deaths, school_closing, workplace_closing, stay_home_restrictions, stringency_index) %>%
  group_by(country_code) %>%
  arrange(date) %>%
  mutate(deaths_day = c(diff(deaths), NA)) %>%
  ungroup() %>%
  arrange(country_code, date) %>%
  mutate(year = year(date), week = isoweek(date))

filter(covid_relevant, country_code == 'DE', week > 10, week < 20) %>% View()
covid_relevant

covid_daily <- select(covid_relevant, country_code, date, deaths, deaths_day,
                      covid_school_closing = school_closing,
                      covid_workplace_closing = workplace_closing,
                      covid_stay_home_restrictions = stay_home_restrictions,
                      covid_stringency_index = stringency_index)

write.csv(covid_daily, 'macrodata/covid19_gov_responses_daily.csv', row.names = FALSE)


getmode <- function(v) {   # chooses first occurrence for multiple modes in v
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

covid_weekly <- group_by(covid_relevant, country_code, year, week) %>%
  summarise(covid_deaths_week = sum(deaths_day),
            covid_school_closing = getmode(school_closing),
            covid_workplace_closing = getmode(workplace_closing),
            covid_stay_home_restrictions = getmode(stay_home_restrictions),
            covid_stringency_index = mean(stringency_index)
  ) %>%
  mutate(date = as.Date(strptime(paste(year, week, '1'), '%Y %W %w'))) %>%
  ungroup() %>%
  filter(complete.cases(.))

View(filter(covid_weekly, country_code == 'DE'))
filter(covid_weekly, country_code == 'DE', week > 10, week < 20)

write.csv(covid_weekly, 'macrodata/covid19_gov_responses_weekly.csv', row.names = FALSE)

##### age at birth data from CIA factbook #####

url_cia <- read_html('macrodata/cia_factbook_ageatbirth.html')

# extract table

country_cia <- url_cia %>% html_nodes(".country") %>% html_text()
age_cia <- url_cia %>% html_nodes(".subfield-number") %>% html_text()
#note_cia <- url_cia %>% html_nodes(".category_data.note") %>% html_text()
date_cia <- url_cia %>% html_nodes(".subfield-date") %>% html_text()


Age_at_Birth <- data.frame(country_cia, age_cia, date_cia, stringsAsFactors = FALSE)
Age_at_Birth$country_cia <- gsub('\\s{2,}', '', Age_at_Birth$country_cia) #remove whitespaces
head(Age_at_Birth)

Age_at_Birth <- mutate(Age_at_Birth,
                       country_cia = ifelse(country_cia == 'Korea, South', 'South Korea', country_cia),
                       age_cia = as.numeric(gsub(' years', '', age_cia)),
                       date_cia = as.integer(regmatches(date_cia, regexpr('\\d{4}', date_cia))))
  
Age_at_Birth_final <- inner_join(Age_at_Birth, countrycodes2names, by = c('country_cia' = 'country_name')) %>%
  select(country_code, age_at_birth = age_cia, age_at_birth_estim_date = date_cia) %>%
  arrange(country_code)

Age_at_Birth_final
countries[!(countries %in% Age_at_Birth_final$country_code)]

write.csv(Age_at_Birth_final, 'macrodata/cia_factbook_ageatbirth.csv', row.names = FALSE)

#### Gender inequality index ####

gii <- read_excel('macrodata/Gender/original data/Gender Inequality Index.xlsx') %>%
  select(Country, gender_ineq_index = `2018`) %>%
  filter(!is.na(gender_ineq_index))
head(gii)

# make sure can be linked via country_name:
sum(!(countrycodes2names$country_name %in% gii$Country))

# no match for 5 countries:
countries[!(countrycodes2names$country_name %in% gii$Country)]

# 3 of these countries are found by different name
# Taiwan and Nigeria are not found
countrycodes2names$country_name_alt <- countrycodes2names$country_name
countrycodes2names[countrycodes2names$country_code %in% c('KR', 'RU', 'VN'), 'country_name_alt'] <- c(
  'Korea (Republic of)',
  'Russian Federation',
  'Viet Nam'
)

# only Taiwan left, all others can be matched
filter(countrycodes2names, !(country_name_alt %in% gii$Country))

# link
gii_prep <- inner_join(countrycodes2names, gii, by = c("country_name_alt" = "Country")) %>%
  select(-country_name, -country_name_alt)
gii_prep

# save
write.csv(gii_prep, 'macrodata/gender_ineq_index.csv')

#### GDP #####

gdp_ppp <- read.csv2("macrodata/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_1120951.csv", skip = 4, sep = ",", header = TRUE, stringsAsFactors = FALSE) %>%
  select(gdp_per_capita = X2018, iso_alpha_3 = Country.Code)
gdp_ppp_final <- mutate(gdp_ppp, country_code = countrycode(gdp_ppp$iso_alpha_3, 'iso3c', 'iso2c')) %>%
  filter(country_code %in% countries) %>%
  select(country_code, gdp_per_capita)

countries[!(countries %in% gdp_ppp_final$country_code)]

write.csv(gdp_ppp_final, 'macrodata/gdp_ppp.csv', row.names = FALSE)

