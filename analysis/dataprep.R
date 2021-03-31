#
# Common data preparation functions.
#
# March 2021, Markus Konrad <markus.konrad@wzb.eu>
#


library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

SELECTED_COUNTRIES_CSV <- '../collection/data/countries.csv'
USERDATA_CSV <- '../collection/data/concat_users.csv'
CONTRIBS_CSV <- '../collection/data/concat_contribs%s.csv'
CONTRIBS_CACHE <- 'cache/contribs%s.RDS'
CONTRIBTIMES_CSV <- '../collection/data/contribtimes_final.csv'


selected_countries <- read.csv(SELECTED_COUNTRIES_CSV, stringsAsFactors = FALSE)$country_code

load_userdata <- function(last_contribs_date) {
  userdata <- read.csv(USERDATA_CSV, stringsAsFactors = FALSE, na.strings = "") %>%
    mutate(gender = as.factor(gender)) %>%
    select(-email, -hireable, -total_contrib)   # determine total contrib. from separate contribs data
  
  userdata <- mutate(userdata,
         days_on_gh = as.integer(date(last_contribs_date) - date(created_at)),
         has_company = !is.na(company) & nchar(str_trim(company)) > 0,
         has_blog = !is.na(blog) & nchar(str_trim(blog)) > 0,
         has_bio = !is.na(bio) & nchar(str_trim(bio)) > 0,
  ) %>% select(-c(company, blog, bio))
  
  # basic checks:
  stopifnot(sum(is.na(userdata$gender)) == 0)
  stopifnot(sum(is.na(userdata$country_code)) == 0)
  
  userdata
}

get_countrycodes2names <- function(userdata) {
  distinct(userdata, country_code, country_name) %>% arrange(country_code) %>% filter(complete.cases(.))
}

load_contribs <- function(aggregtype, from_cache = NULL, remove_week53 = TRUE) {
  csvfile <- sprintf(CONTRIBS_CSV, aggregtype)
  cachefile <- sprintf(CONTRIBS_CACHE, aggregtype)
  
  if (isTRUE(from_cache) || (is.null(from_cache) && file.exists(cachefile))) {
    print(paste('loading from cache', cachefile))
    contribs <- readRDS(cachefile)  # faster loading from RDS file
  } else {
    contribs <- read.csv(csvfile, stringsAsFactors = FALSE) %>% mutate(date = ymd(date))
    stopifnot(all(count(contribs, user_id, date) %>% pull(n) == 1))   # make sure no duplicates
    print(paste('storing to cache', cachefile))
    saveRDS(contribs, cachefile)
  }
  
  if (aggregtype == 'weekly' && remove_week53) {
    contribs <- filter(contribs, week < 53)
  }
  
  #rename(contribs, num_contribs = count)   # TODO
  rename(contribs, num_contribs = sum) %>% select(-mean)
}

load_coviddata <- function(countrycodes, startdate, enddate, cachefile = NULL, min_stay_home_restrictions_level = 1) {
  if (!is.null(cachefile) && file.exists(cachefile)) {
    print(paste('loading from cache', cachefile))
    coviddata <- readRDS(cachefile)  # faster loading from RDS file
  } else {
    coviddata <- COVID19::covid19(countrycodes, level = 1, start = startdate, end = enddate)
    
    if (!is.null(cachefile)) {
      print(paste('storing to cache', cachefile))
      saveRDS(coviddata, cachefile)
    }
  }
  
  if (is.null(min_stay_home_restrictions_level)) {
    coviddata
  } else {
    # see https://covid19datahub.io/articles/doc/data.html#policy-measures-1
    # TODO: specify when lifted; check restrictions level
    coviddata$lockdown <- coviddata$stay_home_restrictions >= min_stay_home_restrictions_level
    
    lockdown_dates <- select(coviddata, date, iso_alpha_2, lockdown) %>%
      filter(lockdown) %>%
      group_by(iso_alpha_2) %>%
      top_n(-1, date) %>%
      ungroup() %>%
      select(iso_alpha_2, date) %>%
      mutate(date_by_month = ymd(paste0(substr(date, 0, 7), '-01')))
    
    lockdown_dates
  }
}

# latest_date: date string in format YYYY-MM-DD; all contributions after this date will be dismissed
prepare_contribs <- function(userdata, contribs, latest_date = NULL, cutoff_quantile = NULL) {
  # contribs users for all contribs
  stopifnot(all(unique(contribs$user_id) %in% userdata$id))
  
  account_creations <- select(userdata, id, created_at) %>% mutate(created_at = ymd(substr(created_at, 1, 10)))
  
  contribs <- left_join(contribs, account_creations, c('user_id' = 'id')) %>%
    filter(date >= created_at) %>%
    select(-created_at)
  
  if (!is.null(latest_date)) {
    contribs <- filter(contribs, date <= ymd(latest_date))
  }
  
  #print('99% to 100% quantiles of contribution counts:')
  #print(quantile(contribs$num_contribs, probs = seq(0.99, 1, by = 0.001)))
  
  if (!is.null(cutoff_quantile)) {
    max_valid_num_contribs <- quantile(contribs$num_contribs, probs = cutoff_quantile)
    
    print(paste0('will remove contributions higher than ', max_valid_num_contribs ,' (', cutoff_quantile * 100, '% quantile)'))
    contribs <- filter(contribs, num_contribs <= max_valid_num_contribs)  # filter extremes
  }
  
  contribs
}

# contribs: should be passed *after* applying prepare_contribs()
prepare_userdata <- function(userdata, contribs, countrycodes2names) {
  contribs_ids <- unique(contribs$user_id)
  
  # find users for which no contributions could be collected
  no_contribs <- select(userdata, id, login, collection_date) %>% filter(!(userdata$id %in% contribs_ids))
  print(paste('will remove', nrow(no_contribs), 'users without contribution data:'))
  print(no_contribs)
  
  # remove users for which no contributions could be fetched
  userdata <- filter(userdata, userdata$id %in% contribs_ids)
  
  # all contributions should be linked with userdata
  stopifnot(all(contribs_ids %in% userdata$id))
  
  # contrib. summaries
  contribs_summ <- group_by(contribs, user_id) %>%
    summarise(contribs_start_date = min(date),
              contribs_end_date = max(date),
              n_days_contrib_recorded = as.integer(contribs_end_date - contribs_start_date),
              user_mean_contrib = mean(num_contribs),
              user_median_contrib = median(num_contribs),
              user_max_contrib = max(num_contribs),
              user_total_contrib = sum(num_contribs)) %>%
    ungroup()
  
  userdata <- left_join(userdata, contribs_summ, by = c('id' = 'user_id'))
  
  stopifnot(sum(is.na(userdata$n_days_contrib_recorded)) == 0)
  stopifnot(sum(userdata$contribs_start_date < ymd(substr(userdata$created_at, 1, 10))) == 0)
  
  # already done in Python script concat_final_data.py:  
  # print('will remove users without predicted gender')
  # userdata <- filter(userdata, !is.na(gender))
  
  # correct country name (is NA in a few cases)
  userdata$country_name <- NULL
  userdata <- left_join(userdata, countrycodes2names, by = 'country_code')
  
  list(
    userdata,
    filter(contribs, user_id %in% userdata$id)
  )
}

filter_countries <- function(userdata, contribs, min_n_users = 30, only_selected_countries = TRUE, req_single_geocoding_result = TRUE) {
  if (only_selected_countries) {
    userdata <- filter(userdata, country_code %in% selected_countries)
  } else {   # already done in Python script concat_final_data.py
    # userdata <- filter(userdata, !is.na(country_code))
  }
  
  if (req_single_geocoding_result) {
    userdata <- filter(userdata, n_geocode_results == 1)
  }
  
  counts_ccode_gender <- count(userdata, country_code, gender) %>% spread(gender, n, fill = 0)
  
  good_ccodes <- filter(counts_ccode_gender, female >= min_n_users, male >= min_n_users) %>%   # require at least min_n_users of *both* genders
    pull(country_code)
  
  userdata <- filter(userdata, country_code %in% good_ccodes)
  
  list(
    userdata,
    filter(contribs, user_id %in% userdata$id),
    counts_ccode_gender
  )
}

# create weekly aggregated data by country and gender
aggregate_per_country_and_gender <- function(userdata, contribs_weekly, add_running_month = TRUE) {
  usercontribs_weekly <- select(userdata, id, country_code, gender) %>%
    inner_join(contribs_weekly, by = c('id' = 'user_id'))
  
  contribs_weekly_gender <- group_by(usercontribs_weekly, country_code, gender, year, week, date) %>%
    summarise(n = n(),
              sum_contribs = sum(num_contribs),
              mean_contribs = mean(num_contribs)) %>%
    ungroup()
  
  if (add_running_month) {
    contribs_weekly_gender <- mutate(contribs_weekly_gender, running_month = (year - min(year)) * 12 + month(date))
  
    stopifnot(min(contribs_weekly_gender$running_month) == 1)
    stopifnot(max(contribs_weekly_gender$running_month) == 12 * 6 + 7)   # 2014 to end of July 2020
  }
  
  contribs_weekly_gender
}

# remove outliers from country-gender aggregated contributions
remove_outliers_from_aggreg_contribs <- function(contribs, set_NAs = FALSE) {
  outlier_limits <- group_by(contribs, country_code, year, gender) %>%
    summarise(country_gender_mean_upr = 1.5 * quantile(mean_contribs, 0.75)) %>%
    ungroup() %>%
    select(country_code, year, gender, country_gender_mean_upr)
  
  contribs <- left_join(contribs, outlier_limits, by = c("country_code", "gender", "year"))
  
  if (set_NAs) {
    contribs <- mutate(contribs,
                       ok = mean_contribs <= country_gender_mean_upr,
                       sum_contribs = ifelse(ok, sum_contribs, NA),
                       mean_contribs = ifelse(ok, mean_contribs, NA)) %>%
                select(-ok)
  } else {
    contribs <- filter(contribs, mean_contribs <= country_gender_mean_upr)
  }
  
  select(contribs, -country_gender_mean_upr)
}


# calc. proportions of contributions and differences in mean contributions
calc_proportions_and_diff_means <- function(contribs_weekly_gender) {
  contribs_weekly_prop <- select(contribs_weekly_gender, country_code, gender, year, running_month, week, date, sum_contribs) %>%
    spread(gender, sum_contribs) %>%
    mutate(prop_sumcontribs = female / male) %>%
    rename(contribs_female = female, contribs_male = male)
  
  contribs_weekly_prop <- bind_cols(contribs_weekly_prop, select(contribs_weekly_gender, country_code, gender, year, week, date, mean_contribs) %>%
                                      spread(gender, mean_contribs) %>%
                                      mutate(diff_meancontribs = female - male) %>%
                                      select(diff_meancontribs))
  
  contribs_weekly_prop <- bind_cols(contribs_weekly_prop, select(contribs_weekly_gender, country_code, gender, year, week, date, n) %>%
                                      spread(gender, n) %>%
                                      mutate(prop_nusers = female / (female + male)) %>%
                                      select(prop_nusers, n_female = female, n_male = male))
  
  contribs_weekly_prop
}

aggregate_contribs <- function(contribs, how, keep_date_components = FALSE) {
  if (how == 'monthly') {
    aggreg_contribs <- group_by(contribs, user_id, year = isoyear(date), month = month(date)) %>%
      summarise(num_contribs = sum(num_contribs)) %>%
      ungroup() %>%
      mutate(date = make_date(year = year, month = month))
      
      if (!keep_date_components) {
        select(aggreg_contribs, -year, -month)
      }
  } else if (how == 'weekly') {
    aggreg_contribs <- group_by(contribs, user_id, year = isoyear(date), week = isoweek(date)) %>%
      summarise(num_contribs = sum(num_contribs)) %>%
      ungroup() %>%
      mutate(date = date(parse_date_time(paste(year, week, '1'), '%Y %W %w', exact = TRUE)))
    
    if (!keep_date_components) {
      select(aggreg_contribs, -year, -week)
    }
  } else {
    stop("'how' must be either 'monthly' or 'weekly'")
  }
  
  aggreg_contribs
}

filter_active_users <- function(userdata, contribs_monthly, startdate, enddate, min_active_months, min_num_contribs = 1) {
  active_user_ids <- filter(contribs_monthly, date >= ymd(startdate), date <= ymd(enddate)) %>%
    group_by(user_id, date) %>%
    mutate(active = num_contribs >= min_num_contribs) %>%
    group_by(user_id) %>%
    summarise(months_active = sum(active)) %>%
    filter(months_active >= min_active_months) %>%
    pull(user_id)
  
  list(
    filter(userdata, id %in% active_user_ids),
    filter(contribs_monthly, user_id %in% active_user_ids)
  )
}

get_covid_gov_resp_start_dates <- function(covid_cov) {
  group_by(covid_gov, country_code) %>%
    filter(covid_stay_home_restrictions > 0 |
             covid_school_closing > 0 |
             covid_workplace_closing > 0) %>%
    summarise(covid_start_date = min(date)) %>%
    ungroup()
}
