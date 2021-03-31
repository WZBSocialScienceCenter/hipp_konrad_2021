#
# Create the final dataset that combines the differences in the GitHub contributions
# forecast (predicted vs. observed) from `forecasts.R` with macrodata that was prepared
# in `prepare_macrodata.R`.
#
# March 2021, Markus Konrad <markus.konrad@wzb.eu>
#


source('dataprep.R')


#### data loading / preparation ####

# country-specific macro data

LOW_OBS_THRESH = 30

# country-level data
ageatbirth <- read.csv('macrodata/cia_factbook_ageatbirth.csv', stringsAsFactors = FALSE)
gdp_ppp <- read.csv('macrodata/gdp_ppp.csv', stringsAsFactors = FALSE)
gender_ineq <- read.csv('macrodata/gender_ineq_index.csv', stringsAsFactors = FALSE) %>% select(-X)
sosurvey <- read.csv('macrodata/stackoverflow_survey_2017-2020_aggreg.csv', stringsAsFactors = FALSE) %>%
  mutate(dev_age_mean = ifelse(dev_age_n >= LOW_OBS_THRESH, dev_age_mean, NA_real_),
         dev_mostly_remote_share = ifelse(dev_mostly_remote_n >= LOW_OBS_THRESH, dev_mostly_remote_share, NA_real_),
         dev_dependents_share = ifelse(dev_dependents_n >= LOW_OBS_THRESH, dev_dependents_share, NA_real_),
         dev_hobby_share = ifelse(dev_hobby_n >= LOW_OBS_THRESH, dev_hobby_share, NA_real_),
         dev_prof_share  = ifelse(dev_prof_n >= LOW_OBS_THRESH, dev_prof_share, NA_real_))

# country-level and time dependent data
covid_gov <- read.csv('macrodata/covid19_gov_responses_weekly.csv', stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date))
covid_start_dates <- get_covid_gov_resp_start_dates(covid_gov)

covid_gov <- left_join(covid_gov, covid_start_dates, by = 'country_code') %>%
  mutate(covid = date >= covid_start_date) %>%
  select(-covid_start_date)

# weekly GitHub contributions data with forecasts

contribs_forecasted <- read.csv('aggregdata/contribs_weekly_forecast.csv', stringsAsFactors = FALSE) %>%
  mutate(gender = as.factor(gender),
         date = ymd(date))
summary(contribs_forecasted)

contribs_diff <- left_join(contribs_forecasted, covid_start_dates, by = 'country_code') %>%
  filter(date >= covid_start_date - weeks(2)) %>%     # plus a few weeks before covid onset
  mutate(diff = actual - pred) %>%
  filter(!is.na(diff)) %>%
  select(country_code, gender, date, diff)
summary(contribs_diff)

countries <- sort(unique(contribs_diff$country_code))   # countries for which we have (enough) contrib. data

#### further prepare macrodata ####

sosurvey_wide <- select(sosurvey, country_code, gender,
                        dev_age_mean, dev_age_n,
                        dev_mostly_remote_share, dev_mostly_remote_n,
                        dev_dependents_share, dev_dependents_n,
                        dev_hobby_share, dev_hobby_n,
                        dev_prof_share, dev_prof_n) %>%
  pivot_wider(names_from = gender, values_from = -c(country_code, gender)) %>%
  mutate(dev_dependents_share = dev_dependents_share_female * dev_dependents_n_female / (dev_dependents_n_female + dev_dependents_n_male)
                              + dev_dependents_share_male * dev_dependents_n_male / (dev_dependents_n_female + dev_dependents_n_male))

# countries w/ low sample size for mean age of female developers and other characteristics:
min_n_thresh <- 30
filter(sosurvey_wide, dev_age_n_female < min_n_thresh)
filter(sosurvey_wide, dev_mostly_remote_n_female < min_n_thresh)
filter(sosurvey_wide, dev_dependents_n_female < min_n_thresh)
filter(sosurvey_wide, dev_hobby_n_female < min_n_thresh)
filter(sosurvey_wide, dev_prof_n_female < min_n_thresh) %>% select(country_code, dev_prof_n_female)

# sosurvey_wide <- filter(sosurvey_wide, dev_age_n_female >= min_n_thresh,
#                         dev_mostly_remote_n_female >= min_n_thresh,
#                         dev_dependents_n_female >= min_n_thresh)

stopifnot(all(countries %in% unique(covid_gov$country_code)))

countrymacro <- left_join(data.frame(country_code = countries, stringsAsFactors = FALSE), ageatbirth, by = 'country_code') %>%
  left_join(gdp_ppp, by = 'country_code') %>%
  left_join(gender_ineq, by = 'country_code') %>%
  left_join(sosurvey_wide, by = 'country_code') %>%
  mutate(diff_firstbirth = age_at_birth - dev_age_mean_female)              # difference between mean age at first birth and mean age of female developers in that country)

countrydata <- left_join(countrymacro, covid_gov, by = 'country_code') %>%
  mutate(date = ymd(date)) %>%
  select(-age_at_birth_estim_date, -starts_with('dev_'), -year, -week,
         dev_age_mean_female, dev_age_mean_male,
         dev_mostly_remote_share_female, dev_mostly_remote_share_male,
         dev_dependents_share, dev_dependents_share_female, dev_dependents_share_male,
         dev_hobby_share_female, dev_hobby_share_male,
         dev_prof_share_female, dev_prof_share_male) %>%
  select(country_code, date, everything())

summary(countrydata)


#### further prepare difference in forecast data ####

contribs_diff_time <- arrange(contribs_diff, country_code, gender, date) %>%
  group_by(country_code, gender) %>%
  #  mutate(time = row_number() - 1) %>%     # time as weeks since start for each country and gender -> doesn't converge, needs to be scaled
  mutate(week = row_number()) %>%     
  ungroup()
contribs_diff_time

# make a balanced panel for all countries:
# number of observation weeks per gender should be equal for all countries

count(contribs_diff_time, country_code, gender)
limit_week_per_gender <- group_by(contribs_diff_time, country_code, gender) %>% summarise(max_week = max(week)) %>%
  ungroup() %>%
  group_by(gender) %>%
  summarise(limit_week = min(max_week))
limit_week_per_gender

contribs_diff_time_balanced <- left_join(contribs_diff_time, limit_week_per_gender, by = 'gender') %>%
  filter(week <= limit_week) %>%
  select(-limit_week)

count(contribs_diff_time_balanced, country_code, gender) %>% left_join(limit_week_per_gender, by = 'gender') %>%
  mutate(check = n == limit_week) %>% pull(check) %>% all() %>% stopifnot()

contribs_diff_time <- group_by(contribs_diff_time, country_code, gender) %>%
  mutate(time = week / n(),   # time as continuous value since start for each country and gender) %>%     
         time2 = time^2) %>%
  ungroup()

contribs_diff_time_balanced <- group_by(contribs_diff_time_balanced, country_code, gender) %>%
  mutate(time = week / n(),   # time as continuous value since start for each country and gender) %>%     
         time2 = time^2) %>%
  ungroup()

group_by(contribs_diff_time_balanced, country_code, gender) %>%
  summarise(min_t = min(time), max_t = max(time)) %>%
  ungroup() %>%
  mutate(check = (min_t == 0.05 & max_t == 1)) %>%
  pull(check) %>% all() %>% stopifnot()


#### generate full final data ####

datavariants <- list(
  unbalanced = contribs_diff_time,
  balanced = contribs_diff_time_balanced
)

for (variant in names(datavariants)) {
  print(variant)
  dataset <- datavariants[[variant]]
  
  finaldata <- left_join(dataset, countrydata, by = c('country_code', 'date')) %>%
    mutate(country_code = as.factor(country_code),
           female = as.integer(gender == 'female'),
           gdp_scaled = scale(gdp_per_capita),
           gii_scaled = scale(gender_ineq_index),
           covid_deaths = covid_deaths_week / max(covid_deaths_week, na.rm = TRUE),
           covid_stringency = covid_stringency_index / 100,
           school_cls = as.integer(covid_school_closing > 0),
           workpl_cls = as.integer(covid_workplace_closing > 0),
           stay_home = as.integer(covid_stay_home_restrictions > 0)
    ) %>%
    select(country_code, date, week, time, time2, gender, female, diff,
           covid_deaths, covid_stringency, covid_school_closing, covid_workplace_closing, covid_stay_home_restrictions,
           school_cls, workpl_cls, stay_home,
           gdp_scaled, gii = gender_ineq_index, gii_scaled, age_at_birth, diff_firstbirth,
           starts_with('dev_')) # %>%
  #filter(complete.cases(.))
  summary(finaldata)
  #View(finaldata)
  
  # removed countries due to missing macrodata:
  setdiff(countries, unique(finaldata$country_code))
  
  write.csv(finaldata, file.path('aggregdata', sprintf('finaldata_%s.csv', variant)), row.names = FALSE)
}
