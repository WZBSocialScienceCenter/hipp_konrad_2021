#
# Create weekly aggregated GitHub data by country and gender.
#
# March 2021, Markus Konrad <markus.konrad@wzb.eu>
#

source('dataprep.R')

#### data loading / preparation ####

LAST_CONTRIBS_DATE = '2020-08-02'

userdata <- load_userdata(last_contribs_date = LAST_CONTRIBS_DATE)

# table that translates ISO country codes to country names
countrycodes2names <- get_countrycodes2names(userdata)
write.csv(countrycodes2names, file.path('aggregdata', 'countrycodes2names.csv'), row.names = FALSE)

num_accounts_cntry_gender <- group_by(userdata, country_code, gender, .drop = FALSE) %>% count()
num_accounts_cntry_gender
write.csv(num_accounts_cntry_gender, file.path('aggregdata', 'num_accounts_cntry_gender.csv'), row.names = FALSE)

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


#### create weekly aggregated data by country and gender ####

contribs_weekly_gender <- aggregate_per_country_and_gender(userdata, contribs_weekly)
contribs_weekly_gender

write.csv(contribs_weekly_gender, file.path('aggregdata', 'contribs_weekly_gender.csv'), row.names = FALSE)
