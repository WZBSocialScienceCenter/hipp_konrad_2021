#
# Descriptive statistics for GitHub user profile data and contributions data at different levels.
#
# March 2021, Markus Konrad <markus.konrad@wzb.eu>
#

source('dataprep.R')
source('plot_utils.R')

library(kableExtra)
library(knitr)

options(knitr.table.format = "latex")

#### data loading / preparation ####

LAST_CONTRIBS_DATE = '2020-08-02'

userdata <- load_userdata(last_contribs_date = LAST_CONTRIBS_DATE)
countrycodes2names <- get_countrycodes2names(userdata)

contribs_weekly <- load_contribs('weekly')
contribs_weekly <- prepare_contribs(userdata, contribs_weekly, latest_date = LAST_CONTRIBS_DATE)

result <- prepare_userdata(userdata, contribs_weekly, countrycodes2names)
userdata <- result[[1]]
contribs_weekly <- result[[2]]
rm(result)

result <- filter_countries(userdata, contribs_weekly, min_n_users = 100)
userdata <- result[[1]]
contribs_weekly <- result[[2]]
counts_countries <- result[[3]]
rm(result)


#### number of contributions in total ####

sum(contribs_weekly$num_contribs)


#### distrib. of contributions ####

usercontribs_weekly <- select(userdata, id, country_code, gender) %>%
  inner_join(contribs_weekly, by = c('id' = 'user_id'))

# num. of contrib. by gender
group_by(usercontribs_weekly, gender) %>% summarise(sum_contribs = sum(num_contribs))

usercontribs_weekly19 <- filter(usercontribs_weekly, year == 2019)
mean(usercontribs_weekly19$num_contribs)
sd(usercontribs_weekly19$num_contribs)

# overall rate of weeks without any contributions in 2019
mean(usercontribs_weekly19$num_contribs == 0)

# rate of weeks without any contributions in 2019 per gender
group_by(usercontribs_weekly19, gender) %>%
  summarise(rate_no_contrib = mean(num_contribs == 0))

# rate of users who contributed in less than 10% of all weeks
group_by(usercontribs_weekly19, id) %>%
  summarise(num_weeks_no_contrib_user = sum(num_contribs == 0)) %>%
  summarise(mean_rate_few_contrib_user = mean(num_weeks_no_contrib_user > 42))

# mean weekly contrib. by gender
meancontribs_gender19 <- group_by(usercontribs_weekly19, gender) %>%
  summarise(mean = mean(num_contribs), sd = sd(num_contribs)) %>%
  pivot_wider(names_from = gender, values_from = c(mean, sd))
meancontribs_gender19

# mean weekly contrib. by gender and country
meancontribs_cntry_gender19 <- group_by(usercontribs_weekly19, country_code, gender) %>%
  summarise(mean = mean(num_contribs), sd = sd(num_contribs)) %>%
  pivot_wider(names_from = gender, values_from = c(mean, sd))
print(meancontribs_cntry_gender19, n=100)

arrange(meancontribs_cntry_gender19, mean_female) %>% head(1)
arrange(meancontribs_cntry_gender19, mean_female) %>% tail(1)
arrange(meancontribs_cntry_gender19, mean_male) %>% head(1)
arrange(meancontribs_cntry_gender19, mean_male) %>% tail(1)

# total number of contributions per user
total_contribs_per_user <- select(usercontribs_weekly, id, gender, country_code, num_contribs) %>%
  group_by(gender, country_code, id) %>%
  summarise(total_contribs = sum(num_contribs)) %>%
  ungroup() %>%
  mutate(no_contribs = total_contribs == 0)

# rate of users without any contrib.
mean(total_contribs_per_user$no_contribs)

# rate of users without any contrib. by gender
group_by(total_contribs_per_user, gender) %>%
  summarise(prop_no_contribs = mean(no_contribs))

# rate of users without any contrib. by country
group_by(total_contribs_per_user, country_code) %>%
  summarise(prop_no_contribs = mean(no_contribs)) %>%
  print(n=100)

# rate of users without any contrib. by gender and country
group_by(total_contribs_per_user, country_code, gender) %>%
  summarise(prop_no_contribs = mean(no_contribs)) %>%
  pivot_wider(names_from = gender, values_from = prop_no_contribs) %>%
  print(n=100)


# distribution of weekly number of contributions per user
ggplot(usercontribs_weekly, aes(x = log1p(num_contribs))) + geom_histogram()
ggplot(usercontribs_weekly, aes(x = log1p(num_contribs))) + geom_histogram() + facet_wrap(~ gender)

# distribution of total number of contributions per user
ggplot(total_contribs_per_user, aes(x = total_contribs)) + geom_histogram() + scale_x_log10()
ggplot(total_contribs_per_user, aes(x = total_contribs, fill = gender)) + geom_density(alpha = 0.5) + scale_x_log10()


#### number of accounts per gender ####

num_accounts_gender <- count(userdata, country_code, gender) %>%
  pivot_wider(names_from = gender, values_from = n) %>%
  mutate(total = female + male, prop_female = female/ total)
num_accounts_gender

summary(num_accounts_gender$prop_female)

write.csv(num_accounts_gender, file.path('summarydata', 'num_accounts_gender.csv'), row.names = FALSE)


#### create weekly aggregated data by country and gender ####

contribs_weekly_gender <- aggregate_per_country_and_gender(userdata, contribs_weekly)
contribs_weekly_gender_with_NAs <- remove_outliers_from_aggreg_contribs(contribs_weekly_gender, set_NAs = TRUE)


#### weekly num. of observations per country and gender ####

n_obs_country_gender <- filter(contribs_weekly_gender_with_NAs, !is.na(mean_contribs)) %>%
  select(-c(year, week, sum_contribs, mean_contribs, running_month)) %>%
  pivot_wider(names_from = gender, values_from = n) %>%
  mutate(female = ifelse(is.na(female), 0, female),
         male = ifelse(is.na(male), 0, male))
n_obs_country_gender


#### summary table (table 2 in paper) ####

covid_gov <- read.csv('macrodata/covid19_gov_responses_weekly.csv', stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date))
covid_start_dates <- get_covid_gov_resp_start_dates(covid_gov)

covid_gov <- left_join(covid_gov, covid_start_dates, by = 'country_code') %>%
  filter(date >= covid_start_date - weeks(2), date <= max(contribs_weekly_gender$date)) %>%
  select(-covid_start_date)

policy_summary <- select(covid_gov, country_code, covid_school_closing, covid_workplace_closing, covid_stay_home_restrictions) %>%
  pivot_longer(starts_with('covid_')) %>%
  mutate(name = substr(name, 7, nchar(name))) %>%
  count(country_code, name, value) %>%
  pivot_wider(names_from = c(name, value), values_from = n, values_fill = 0L)

policy_summary <- policy_summary[,order(colnames(policy_summary))]

stringency_summary <- group_by(covid_gov, country_code) %>%
  summarise(mean_stringency = mean(covid_stringency_index), sd_stringency = sd(covid_stringency_index))

policy_summary <- left_join(policy_summary, stringency_summary, by = 'country_code')

select(policy_summary,
       country_code,
       starts_with('school_closing_'),
       starts_with('workplace_closing_'),
       starts_with('stay_home_restrictions_'),
       everything()) %>%
  kable(booktabs = TRUE,
        digits = 0,
        caption = 'Overview on lockdown intensity (number of weeks at certain policy measure levels and mean stringency index)',
        col.names = c('', rep(0:3, 3), 'M', '(sd)')) %>%
  add_header_above(c('', 'school closings' = 4, 'workplace closings' = 4, 'curfews' = 4, 'overall stringency' = 2)) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%
  write(file = 'tables/table_policy_measures_summary.tex')


