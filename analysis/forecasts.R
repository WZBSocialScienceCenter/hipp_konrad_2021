#
# Create forecasts from aggregated GitHub contributions time series data.
#
# March 2021, Markus Konrad <markus.konrad@wzb.eu>
#

source('dataprep.R')
source('plot_utils.R')

library(imputeTS)


#### data loading / preparation ####

contribs_weekly_gender <- read.csv(file.path('aggregdata', 'contribs_weekly_gender.csv'), stringsAsFactors = FALSE)

# set outlier in a week as NA
contribs_weekly_gender_with_NAs <- remove_outliers_from_aggreg_contribs(contribs_weekly_gender, set_NAs = TRUE) %>%
  mutate(week_fct = factor(week))

# get country-specific COVID onset date as defined in get_covid_gov_resp_start_dates()
covid_gov <- read.csv(file.path('macrodata', 'covid19_gov_responses_weekly.csv'), stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date))
covid_start_dates <- get_covid_gov_resp_start_dates(covid_gov)

# countries in sample
obs_countries <- unique(contribs_weekly_gender$country_code) %>% sort()
obs_countries


#### generate forecasts: functions ####

# function to fit a HoltWinters model to `ts_contribs_before` and make a prediction from it until end of `ts_contribs_full`
hwmodel_forecast_ts <- function(ts_contribs_before, ts_contribs_full, seasonal) {
  hwmodel <- HoltWinters(ts_contribs_before, start.periods = 4, seasonal = seasonal, beta = FALSE)
  
  pred <- predict(hwmodel, n.ahead = length(ts_contribs_full) - length(ts_contribs_before))
  pred_full <- ts(c(hwmodel$fitted[, 'xhat'], pred),
                  start = start(hwmodel$fitted),
                  frequency = frequency(hwmodel$fitted))
  
  list(
    model = hwmodel,
    prediction = pred_full
  )
}

# function that creates a Holt-Winters forecast model for female and male time series data in `contribs_gender`
# and performs a prediction
create_hwmodel_forecasts <- function(contribs_gender) {
  contribs_gender <- select(contribs_gender, date, gender, covid, y = mean_contribs)
  seasonal <- 'add'  
  bygender <- spread(contribs_gender, gender, y)
  
  # full ts per gender, freq. defined by 52 weeks in a year
  contribs_f <- pull(bygender, female) %>% ts(frequency = 52) %>% na_interpolation()
  contribs_m <- pull(bygender, male) %>% ts(frequency = 52) %>% na_interpolation()
  stopifnot(length(contribs_f) == length(contribs_m))
  
  # ts until COVID indicator
  until_index <- max(which(!bygender$covid))
  stopifnot(until_index < length(contribs_f))
  contribs_f_before <- window(contribs_f, c(1, 1), time(contribs_f)[until_index])
  contribs_m_before <- window(contribs_m, c(1, 1), time(contribs_m)[until_index])
  
  # make forecasts for female and male contrib.
  forecast_f <- hwmodel_forecast_ts(contribs_f_before, contribs_f, seasonal = seasonal)
  forecast_m <- hwmodel_forecast_ts(contribs_m_before, contribs_m, seasonal = seasonal)
  
  # set timeseries into relation: difference of means
  relation_actual <- contribs_f - contribs_m
  relation_pred <- forecast_f$prediction - forecast_m$prediction
  
  # convert prediction per gender timeseries to dataframe
  pred_long <- data.frame(date = rep(bygender$date, 2),
                          gender = factor(c(rep('female', nrow(bygender)), rep('male', nrow(bygender)))),
                          y_pred = c(rep(NA, 52), as.numeric(forecast_f$prediction),    # first year (52 weeks) not predicted
                                     rep(NA, 52), as.numeric(forecast_m$prediction)),
                          stringsAsFactors = FALSE)
  
  # combine actual and predicted values
  actual_and_pred_by_gender <- left_join(contribs_gender, pred_long, by = c('date', 'gender')) %>%
    select(gender, date, actual = y, pred = y_pred) %>%
    pivot_longer(c(actual, pred)) %>%
    mutate(gender = as.factor(gender),
           date = date(date),
           name = as.factor(name))
  

  # return
  list(
    forecast_f = forecast_f,
    forecast_m = forecast_m,
    relation_actual = relation_actual,
    relation_pred = relation_pred,
    actual_and_pred_by_gender = actual_and_pred_by_gender
  )
}


# function that creates a Holt-Winters forecast model for a specific country's time series data and plots the model's forecast
holtwinters_forecast_model_and_plot <- function(ccode, plots_dir, contribs_weekly_gender_cntry_with_NAs,  intervention_date, plot_startdate = '2018-01-01') {
  mean_forecasts <- create_hwmodel_forecasts(contribs_weekly_gender_cntry_with_NAs)
  
  p_gender_means <- plot_forecast_gender(mean_forecasts$actual_and_pred_by_gender,
                                         xintercept = intervention_date,
                                         title = paste(ccode, 'forecast of weekly mean of contrib.'),
                                         startdate = plot_startdate,
                                         ylab = 'mean weekly contrib.')

  res <- list(
    model_plot = p_gender_means,
    mean_forecasts = mean_forecasts
  )
  
  ggsave(file.path(plots_dir, paste0(ccode, '_forecast_holtwinters.png')),
         res$model_plot,
         width = 8, height = 4)
  
  res
}


#### generate forecasts: perform model fitting and predictions per country ####

forecastdata <- list()
for (ccode in obs_countries) {
  print(ccode)
  
  intervention_date <- filter(covid_start_dates, country_code == ccode) %>% pull(covid_start_date)
  stopifnot(length(intervention_date) == 1)
  
  plots_dir <- file.path("plots", ccode)
  dir.create(plots_dir, showWarnings = FALSE)
  
  # country data
  contribs_weekly_gender_cntry_with_NAs <- filter(contribs_weekly_gender_with_NAs, country_code == ccode) %>%
    select(-country_code) %>%
    mutate(covid = date >= intervention_date,
           month_fct = factor(month(date)),
           year_fct = factor(year))
  
  # Holt-Winters forecast model
  hwres <- holtwinters_forecast_model_and_plot(ccode, plots_dir, contribs_weekly_gender_cntry_with_NAs,
                                               intervention_date = intervention_date, plot_startdate = '2018-01-01')
  
  # concatenate data
  forecastdata <- c(
    forecastdata,
    list(mutate(hwres$mean_forecasts$actual_and_pred_by_gender, country_code = ccode))
  )
}

#### combine to full forecast data for all countries ####

forecasted_full <- bind_rows(forecastdata) %>%
  select(country_code, everything()) %>%
  pivot_wider(names_from = name, values_from = value)

forecasted_full

write.csv(forecasted_full, 'aggregdata/contribs_weekly_forecast.csv', row.names = FALSE)

