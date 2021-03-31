#
# Script to generate plots and tables used in the paper.
#
# March 2021, Markus Konrad <markus.konrad@wzb.eu>
#


source('dataprep.R')
source('plot_utils.R')

library(kableExtra)
library(knitr)

options(knitr.table.format = "latex")

#### data loading / preparation ####

contribs_forecasted <- read.csv('aggregdata/contribs_weekly_forecast.csv', stringsAsFactors = FALSE) %>%
  mutate(gender = as.factor(gender),
         date = ymd(date))
summary(contribs_forecasted)

covid_gov <- read.csv('macrodata/covid19_gov_responses_weekly.csv', stringsAsFactors = FALSE) %>%
  mutate(date = ymd(date))
covid_start_dates <- get_covid_gov_resp_start_dates(covid_gov)

contribs_diff <- left_join(contribs_forecasted, covid_start_dates, by = 'country_code') %>%
  filter(date >= covid_start_date - weeks(2)) %>%     # plus a few weeks before covid onset
  mutate(diff = actual - pred) %>%
  filter(!is.na(diff)) %>%
  select(country_code, gender, date, diff)
summary(contribs_diff)

countries <- sort(unique(contribs_diff$country_code))   # countries for which we have (enough) contrib. data

countrycodes2names <- read.csv(file.path('aggregdata', 'countrycodes2names.csv'), stringsAsFactors = FALSE) %>%
  filter(country_code %in% countries)
num_accounts_cntry_gender <- read.csv(file.path('aggregdata', 'num_accounts_cntry_gender.csv'), stringsAsFactors = FALSE) %>%
  filter(country_code %in% countries) %>%
  left_join(countrycodes2names, by = 'country_code')

country_categories <- list(
  BRIC = c('BR', 'RU', 'IN', 'CN'),
  Europe = c('DE', 'IT', 'ES', 'GB'),
  'Other rich democracies' = c('US', 'KR')
)

selected_countries <- c('BR', 'CN', 'DE', 'GB', 'IN', 'IT', 'RU', 'KR', 'ES', 'US')


#### plot: actual contributions vs. forecasts ####

plotdata_actual_forec <- filter(contribs_forecasted, country_code %in% selected_countries, date >= '2018-01-01') %>%
  left_join(countrycodes2names, by = 'country_code') %>%
  left_join(covid_start_dates, by = 'country_code') %>%
  pivot_longer(c(actual, pred))

# # this custom labeller makes sure that the country label only appears in the
# # left facet ("female" facet) and that the gender labels only appear in the
# # first row of facets
# firstrow_aware_facet_labeller <- function(labels) {
#   newlabels <- mutate(labels,
#                       country_rank = group_indices(labels, country_code),
#                       label1 = ifelse(gender == 'female', country_code, ''),
#                       label2 = ifelse(country_rank == 1, as.character(gender), ''),
#                       label = str_trim(paste(label1,
#                                              ifelse(label2 == 'diff', 'difference (female - male)', label2),
#                                              sep = '\n\n'))) %>%
#     pull(label)
#   list(`1` = newlabels)
# }

actual_forec_plots <- list()
#actual_forec_pal <- scales::hue_pal()(2)    # predicted, actual colors
actual_forec_pal <- scales::grey_pal(0.4, 0.7)(2)[2:1]

obs_dates <- unique(plotdata_actual_forec$date)
nlabels_date <- sort(obs_dates)[floor(length(obs_dates)/2)]

for (cntry_cat in names(country_categories)) {
  cat_cntrs <- country_categories[[cntry_cat]]
  
  cat_plt_data <- filter(plotdata_actual_forec, country_code %in% cat_cntrs) %>%
    mutate(country_code = as.factor(country_code))
  
  labels_y_pos <- group_by(cat_plt_data, country_code) %>%
    summarise(y = 1.2 * max(value, na.rm = TRUE)) %>%
    ungroup()
  
  nlabels <- filter(num_accounts_cntry_gender, country_code %in% cat_cntrs) %>%
    mutate(country_code = as.factor(country_code), label = paste0('N = ', n)) %>%
    left_join(labels_y_pos, by = 'country_code')
  
  if (which(cntry_cat == names(country_categories)) == length(country_categories)) {
    show_guide <- guide_legend()
  } else {
    show_guide <- 'none'
  }
  
  p <- ggplot(cat_plt_data, aes(x = date, y = value)) +
    geom_line(aes(color = name)) +
    geom_vline(aes(xintercept = covid_start_date), linetype = 'dashed') +
    geom_text(data = nlabels, aes(x = nlabels_date, y = y, label = label), hjust = 0.5, vjust = 1, size = 2.5) +
    scale_color_manual(values = actual_forec_pal,
                       limits = c('pred', 'actual'),
                       labels = c('pred' = 'predicted', 'actual' = 'actual'),
                       name = '',
                       guide = show_guide) +
    labs(title = cntry_cat, x = '', y = '') +
    facet_grid(country_name ~ gender, scales = 'free_y') +
    theme(plot.title = element_text(hjust = 0, size = rel(0.9)),
          legend.position = 'bottom')
  
  actual_forec_plots[[cntry_cat]] <- p
}

(p_actual_forec <- wrap_plots(actual_forec_plots) +
  plot_layout(ncol = 1, heights = sapply(country_categories, length))) # +
  # plot_annotation(
  #   #title = 'Forecasted and actual development of mean number of weekly contributions',
  #   title = sprintf(
  #     '<strong style="color:%s">Predicted</strong> and <strong style="color:%s">actual</strong>
  #      mean number of weekly contributions. Vertical dashed line marks COVID-19 onset date.',
  #     actual_forec_pal[1], actual_forec_pal[2]
  #   ),
  #   theme = theme(plot.title = element_markdown())
  # ))
ggsave('articleplots/forecast_vs_actual.png', p_actual_forec, width = 8, height = 12)


#### plot: actual contributions vs. forecasts -- big plot for appendix w/ all countries ####

plotdata_actual_forec_apdx <- filter(contribs_forecasted, date >= '2018-01-01') %>%
  left_join(countrycodes2names, by = 'country_code') %>%
  left_join(select(countrycode::codelist, iso2c, continent), by = c('country_code' = 'iso2c')) %>%
  left_join(covid_start_dates, by = 'country_code') %>%
  pivot_longer(c(actual, pred))

# actual_forec_apdx_plots <- list()
continents <- sort(unique(plotdata_actual_forec_apdx$continent))
for (contidx in 1:length(continents)) {
  cont <- continents[contidx]
  
  plotdata_continent <- filter(plotdata_actual_forec_apdx, continent == cont)
  p <- ggplot(plotdata_continent, aes(x = date, y = value)) +
    geom_line(aes(color = name)) +
    geom_vline(aes(xintercept = covid_start_date), linetype = 'dashed') +
    scale_color_manual(values = actual_forec_pal,
                       limits = c('pred', 'actual'),
                       labels = c('pred' = 'predicted', 'actual' = 'actual'),
                       name = '',
                       guide = 'none') +
    facet_grid(country_name ~ gender, scales = 'free_y') +
    theme(plot.title = element_text(hjust = 0),
          plot.subtitle = element_markdown(),
          strip.text = element_text(size = rel(0.6)))
  
  if (contidx == 1) {
    p <- p + labs(title = paste0('Forecasted and actual development of mean number\nof weekly contributions\n\n', cont, '\n'),
                  subtitle = sprintf(
                    '<strong style="color:%s">Predicted</strong> and <strong style="color:%s">actual</strong>
                     mean number of weekly contributions.<br>Vertical dashed line marks COVID-19 onset date.',
                    actual_forec_pal[1], actual_forec_pal[2]
                  ),
                  x = '',
                  y = '')
    addheight <- 3
  } else {
    p <- p + labs(title = cont, x = '', y = '')
    addheight <- 1
  }
  
  ggsave(sprintf('articleplots/appendix_forecast_vs_actual_%s.png', cont),
         p, width = 5, height = addheight + 0.75 * length(unique(plotdata_continent$country_name)))
  # actual_forec_apdx_plots[[cont]] <- p
}

# subplot_heights <- distinct(plotdata_actual_forec_apdx, continent, country_code) %>%
#   count(continent) %>%
#   arrange(continent) %>%
#   pull(n)

# p_actual_forec_apdx <- wrap_plots(actual_forec_apdx_plots, guides = 'collect') +
#     plot_layout(ncol = 1, heights = subplot_heights) +
#     plot_annotation(
#       title = 'Forecasted and actual development of mean number of weekly contributions',
#       subtitle = sprintf(
#         '<strong style="color:%s">Predicted</strong> and <strong style="color:%s">actual</strong>
#        mean number of weekly contributions. Vertical dashed line marks COVID-19 onset date.',
#         actual_forec_pal[1], actual_forec_pal[2]
#       ),
#       theme = theme(plot.subtitle = element_markdown())
#     )
# ggsave('articleplots/appendix_forecast_vs_actual_all.png', p_actual_forec_apdx, width = 8, height = 34)

#### plot: trends in forecast deviations categorized ####

brewerpal <- RColorBrewer::brewer.pal(3, 'Dark2')
gender_pal <- c(brewerpal[1], brewerpal[2])    # female, male


forecastdiff_cntrs <- left_join(contribs_diff, countrycodes2names, by = 'country_code')

categorize_param <- function(x, p, thresh = 0.05) {
  case_when(p <= thresh & x > 0 ~ 'up',
            p <= thresh & x < 0 ~ 'down',
            TRUE ~ 'const')
}

paramlevels <- c('up', 'const', 'down')
latexarrows_icept <- c('$\\uparrow$', '$\\rightarrow$', '$\\downarrow$')
latexarrows_slope <- c('$\\nearrow$', '$\\rightarrow$', '$\\searrow$')

cntry_gender_trends <- group_by(forecastdiff_cntrs, country_code, gender) %>% group_modify(~ {
    grpdata <- .
    grpdata$week <- 1:nrow(grpdata)
    m <- lm(diff ~ week, data = grpdata)
    res <- as.data.frame(summary(m)$coefficients[, c(1, 4)])
    rownames(res) <- NULL
    res$param <- c('icept', 'slope')
    res <- pivot_wider(res, names_from = param, values_from = -param)
    colnames(res) <- c('icept', 'slope', 'icept_pvalue', 'slope_pvalue')
    res
  }) %>%
  ungroup() %>%
  mutate(icept_categ = factor(categorize_param(icept, icept_pvalue), levels = paramlevels),
         slope_categ = factor(categorize_param(slope, slope_pvalue), levels = paramlevels)) %>%
  mutate(icept_categ_lvl = as.integer(icept_categ),
         slope_categ_lvl = as.integer(slope_categ))

ggplot(cntry_gender_trends, aes(x = icept, y = slope, color = gender)) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(alpha = 0.25) +
  scale_color_manual(values = gender_pal,
                     limits = c('female', 'male'),
                     name = '')

cntry_gender_trends_categ <-
  select(cntry_gender_trends, country_code, gender, ends_with('_categ')) %>%
  pivot_wider(names_from = gender, values_from = ends_with('_categ')) %>%
  mutate(categ_label = paste(icept_categ_female, slope_categ_female, icept_categ_male, slope_categ_male, sep = '_'))

arrange(cntry_gender_trends_categ, icept_categ_female, slope_categ_female, icept_categ_male, slope_categ_male, country_code) %>%
  select(icept_fem = icept_categ_female, slope_fem = slope_categ_female, icept_male = icept_categ_male, slope_male = slope_categ_male, country = country_code) %>%
  print(n=100)

cntry_gender_trends_categ_counts <- count(cntry_gender_trends_categ, categ_label) %>%
  arrange(desc(n))
cntry_gender_trends_categ_counts

cntry_gender_trends_tabledata <- select(cntry_gender_trends, country_code, gender, ends_with('_categ_lvl')) %>%
  pivot_wider(names_from = gender, values_from = ends_with('_categ_lvl')) %>%
  mutate(group_id = 1000 * icept_categ_lvl_female
                  +  100 * slope_categ_lvl_female
                  +   10 * icept_categ_lvl_male
                  +        slope_categ_lvl_male) %>%
  select(group_id, icept_categ_lvl_female, slope_categ_lvl_female, icept_categ_lvl_male, slope_categ_lvl_male, country_code) %>%
  left_join(countrycodes2names, by = 'country_code') %>%
  left_join(select(countrycode::codelist, iso2c, continent), by = c('country_code' = 'iso2c')) %>%
  #arrange(group_id, country_name) %>%
  arrange(country_name, group_id) %>%
  select(-c(group_id, country_code, continent)) %>%
  mutate(icept_categ_lvl_female = latexarrows_icept[icept_categ_lvl_female],
         slope_categ_lvl_female = latexarrows_slope[slope_categ_lvl_female],
         icept_categ_lvl_male = latexarrows_icept[icept_categ_lvl_male],
         slope_categ_lvl_male = latexarrows_slope[slope_categ_lvl_male])

cntry_gender_trends_tabledata

kable(cntry_gender_trends_tabledata,
      booktabs = TRUE,
      caption = 'Categorization of trends per country',
      col.names = c(rep(c('intercept', 'slope'), 2), ''),
      escape = FALSE) %>%
  add_header_above(c('female' = 2, 'male' = 2, 'country')) %>%
  kable_styling(latex_options = c("hold_position", "striped")) %>%   #, font_size = 6) %>%    # \scriptsize needs to be added
  #collapse_rows(columns = 1:4, valign = 'top', latex_hline = 'none') %>%
  write(file = 'tables/table_trends.tex')

# and remove \addlinespace

trend_description <- function(icept_categ, slope_categ) {
  # icept_descr <- case_when(icept_categ == 'up' ~ 'initial upswing',
  #                          icept_categ == 'down' ~ 'initial slump',
  #                          TRUE ~ 'initially no change')
  # slope_descr <- case_when(slope_categ == 'up' ~ 'upwards trend',
  #                          slope_categ == 'down' ~ 'downwards trend',
  #                          TRUE ~ 'staying constant')
  icept_descr <- case_when(icept_categ == 'up' ~ '↑',
                           icept_categ == 'down' ~ '↓',
                           TRUE ~ '→')
  slope_descr <- case_when(slope_categ == 'up' ~ '↗',
                           slope_categ == 'down' ~ '↘',
                           TRUE ~ '→')
  
  
  sprintf('%s, then %s', icept_descr, slope_descr)
}

plot_cntry_gender_trends <- function(grp) {
  trend_descr_female <- trend_description(unique(grp$icept_categ_female), unique(grp$slope_categ_female))
  trend_descr_male <- trend_description(unique(grp$icept_categ_male), unique(grp$slope_categ_male))
  stopifnot(length(trend_descr_female) == 1)
  stopifnot(length(trend_descr_male) == 1)
  
  p <- ggplot(grp, aes(x = date, y = diff, color = gender)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_point(alpha = 0.6) +
    geom_smooth(method = 'lm', level = 0.9) +
    #scale_color_brewer(palette = 'Dark2', name = '') +
    scale_color_manual(values = gender_pal,
                       limits = c('female', 'male'),
                       name = '',
                       guide = 'none') +
    labs(title = sprintf('<strong style="color:%s">Female: %s</strong><br><strong style="color:%s">Male: %s</strong>',
                         gender_pal[1], trend_descr_female, gender_pal[2], trend_descr_male),
         x = '', y = '') +
    facet_wrap(~ country_name, scales = 'free_y') +
    theme(plot.title = element_markdown(hjust = 0.5, size = rel(0.9)))
  
  tibble(plot = list(p))
}

cntry_gender_trend_plots <- filter(cntry_gender_trends_categ_counts, n > 1) %>%
  select(categ_label, num_cntrs_in_categ = n) %>%
  left_join(cntry_gender_trends_categ, by = 'categ_label') %>%
  left_join(forecastdiff_cntrs, by = 'country_code') %>%
  arrange(desc(num_cntrs_in_categ)) %>%
  group_by(categ_label) %>%
  group_modify(~plot_cntry_gender_trends(.)) %>%
  left_join(cntry_gender_trends_categ_counts, by = 'categ_label') %>%   # because sorting is not retained
  arrange(desc(n))

(p_forecastdiff_categ <- wrap_plots(cntry_gender_trend_plots$plot) +
  plot_layout(ncol = 3, heights = c(2, 1, 1)) +
  plot_annotation(title = 'Trends in contributions'))

ggsave(sprintf('articleplots/forecastdiff_categ.png'),
       p_forecastdiff_categ, width = 14, height = 10)


#### plot: trends in forecast deviations for selected countries ####

plotdata_forecastdiff <- filter(contribs_diff, country_code %in% selected_countries) %>%
  left_join(countrycodes2names, by = 'country_code')

forecastdiff_plots <- list()
smooth_method <- 'loess'

for (cntry_cat in names(country_categories)) {
  p <- filter(plotdata_forecastdiff, country_code %in% country_categories[[cntry_cat]]) %>%
    ggplot(aes(x = date, y = diff, color = gender)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_point(alpha = 0.6) +
    geom_smooth(method = smooth_method) +
    #scale_color_brewer(palette = 'Dark2', name = '') +
    scale_color_manual(values = gender_pal,
                       limits = c('female', 'male'),
                       name = '',
                       guide = 'none') +
    labs(title = cntry_cat, x = '', y = '') +
    facet_wrap(~ country_name, scales = 'free_y', ncol = 3) +
    theme(plot.title = element_text(hjust = 0, size = rel(0.9)))
  
  forecastdiff_plots[[cntry_cat]] <- p
}

(p_forecastdiff <- wrap_plots(forecastdiff_plots, guides = 'collect') +
  plot_layout(ncol = 1, heights = ceiling(sapply(country_categories, length) / 3)) +
  plot_annotation(
    title = 'Differences to forecast',
    subtitle = sprintf(
      'Differences to forecasted mean number of weekly contributions for 
       <strong style="color:%s">female</strong> and <strong style="color:%s">male</strong>
       GitHub accounts,<br>starting two weeks before COVID-19 onset date.',
      gender_pal[1], gender_pal[2]
    ),
    theme = theme(plot.subtitle = element_markdown())
  )
)

ggsave(sprintf('articleplots/forecastdiff_%s.png', smooth_method),
       p_forecastdiff, width = 8, height = 8)
