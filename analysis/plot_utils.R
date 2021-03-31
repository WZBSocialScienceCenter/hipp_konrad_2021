#
# Plotting utility functions.
#
# March 2021, Markus Konrad <markus.konrad@wzb.eu>
#

require(ggplot2)
require(ggtext)
require(patchwork)


theme_set(theme_minimal())

add_labels <- function(title, subtitle = '', xaxislab = NULL, yaxislab = NULL) {
  # if (!is.null(collection_time)) {
  #   caption <- paste('source: data fetched from GitHub API between',
  #                    collection_time[1], 'and', collection_time[2])
  # } else {
  #   caption <- NULL
  # }
  
  labs(title = title,
       subtitle = subtitle,
       # caption = caption,
       x = xaxislab, y = yaxislab)
}


plot_country_facets_ts <- function(data, y, title, color_by_gender, plot_ribbon = FALSE, ymin = NULL, ymax = NULL, facet_by_gender = FALSE,
                                   filter_country_codes = NULL, figname = NULL, figsize = NULL) {
  y <- enquo(y)
  
  if (is.null(filter_country_codes)) {
    fileprefix <- 'all_countries_'
    if (is.null(figsize)) {
      figsize <- c(8, 12)
    }
  } else {
    fileprefix <- 'selected_countries_'
    data <- filter(data, country_code %in% filter_country_codes)
    title <- paste(title, 'for selected countries')
    if (is.null(figsize)) {
      figsize <- c(8, 6)
    }
  }
  
  p <- ggplot(data, aes(x = date, y = !!y)) +
    labs(title = title, x = '', y = '') +
    theme_minimal()
  
  if (color_by_gender) {
    p <- p + geom_line(aes(color = gender)) +
      scale_color_brewer(palette = 'Dark2', guide = ifelse(facet_by_gender, 'none', 'legend'))
  } else {
    p <- p + geom_line()
  }
  
  if (plot_ribbon) {
    ymin <- enquo(ymin)
    ymax <- enquo(ymax)
    
    if (color_by_gender) {
      ribbon_mapping <- aes(ymin = !!ymin, ymax = !!ymax, fill = gender)
    } else {
      ribbon_mapping <- aes(ymin = !!ymin, ymax = !!ymax)
    }
    
    p <- p + geom_ribbon(ribbon_mapping, alpha = 0.2)
    
    if (color_by_gender) {
      p <- p + scale_fill_brewer(palette = 'Dark2', guide = 'none')
    }
  }
  
  if (facet_by_gender) {
    p <- p + facet_wrap(country_code ~ gender, scales = 'free_y', ncol = 2)
  } else {
    p <- p + facet_wrap(~ country_code, ncol = 3, scales = 'free_y')
  }
  
  if (!is.null(figname)) {
    plotfile <- file.path('plots', paste0(fileprefix, figname))
    print(paste('will save plot to', plotfile))
    ggsave(plotfile, p, width = figsize[1], height = figsize[2])
  }
  
  p
}


plot_country_total_contribs <- function(countrydata, title, y = sum_contribs, ylab = 'total contributions') {
  y <- enquo(y)
  
  ggplot(countrydata, aes(x = date, y = !!y, color = gender)) +
    geom_line(alpha = 0.25) +
    geom_point(alpha = 0.25) +
    scale_color_brewer(palette = 'Dark2', guide = 'none') +
    geom_smooth(method = 'loess', span = 0.25) +
    facet_wrap(~ gender, ncol = 1, scales = 'free_y') +
    labs(title = title, x = '', y = ylab) +
    theme_minimal()
}

plot_country_prop <- function(countrydata, title, y = prop_sumcontribs, ylab = 'prop. of female contributions') {
  y <- enquo(y)
  
  ggplot(countrydata, aes(x = date, y = !!y)) +
    geom_line() +
    geom_point() +
    labs(title = title, x = '', y = ylab) +
    theme_minimal()
}

plot_country_monthly_trends <- function(countrydata, title, facets = TRUE) {
  yearly_std <- group_by(countrydata, gender, year) %>%
    mutate(sum_contribs_centered = scale(sum_contribs, center = TRUE, scale = FALSE),
           sum_contribs_std = scale(sum_contribs, center = TRUE, scale = TRUE),
           year2020 = as.integer(year == 2020)) %>%
    ungroup()
  
  if (facets) {
    ggplot(yearly_std, aes(x = month_fct, y = sum_contribs_std, color = gender, group = year)) +
      geom_line() +
      geom_point() +
      scale_color_brewer(palette = 'Dark2', guide = FALSE) +
      facet_grid(rows = vars(year), cols = vars(gender)) +
      labs(title = title, x = 'month', y = 'standardized total contributions') +
      theme_minimal()
  } else {
    ggplot(yearly_std, aes(x = month_fct, y = sum_contribs_centered, color = year, group = year, alpha = year2020)) +
      geom_line() +
      geom_point() +
      scale_color_viridis_c(direction = -1) +
      scale_alpha_continuous(range = c(0.2, 1), guide = FALSE) +
      facet_wrap(~ gender, ncol = 1, scales = 'free_y') +
      labs(title = title, x = 'month', y = 'centered total contributions') +
      theme_minimal()
  }
}

plot_predictions <- function(pred, y = sum_contribs, title = 'model predictions', ylab = '', xintercept = NULL, ribbon = TRUE, vertical = TRUE) {
  y <- enquo(y)
  
  if (vertical) {
    facetrows <- 2
    facetcols <- 1
  } else {
    facetrows <- 1
    facetcols <- 2
  }
  
  pred_long <- select(pred, gender, date, observed = !!y, fit) %>%
   gather("type", "value", -c(gender, date))
  
  p <- ggplot() +
    geom_point(aes(x = date, y = !!y, color = gender), alpha = 0.5, shape = 21, data = pred)
  
  if (ribbon) {
    p <- p + geom_ribbon(aes(x = date, ymin = lwr, ymax = upr, fill = gender), alpha = 0.2, data = pred)
  }
  
  p <- p +
    geom_line(aes(x = date, y = value, color = gender, linetype = type), alpha = 0.5, data = pred_long) +
    scale_color_brewer(palette = 'Dark2', guide = 'none') +
    scale_fill_brewer(palette = 'Dark2', guide = 'none') +
    scale_linetype_discrete(limits = c('observed', 'fit')) +
    facet_wrap(~ gender, nrow = facetrows, ncol = facetcols, scales = 'free_y') +
    labs(title = title, x = '', y = ylab) +
    theme_minimal()
  
  if (!is.null(xintercept)) {
    p <- p + geom_vline(xintercept = xintercept, linetype = 'dashed')
  }
  
  p
}

plot_partial_predictions <- function(pred, title = 'model predictions', ylab = '', xintercept = NULL) {
  w_gender <- 'gender' %in% colnames(pred)
  
  if (w_gender) {
    mapping <- aes(x = date, y = fit, color = gender)
  } else {
    mapping <- aes(x = date, y = fit)
  }
  
  p <- ggplot(pred, mapping) +
    geom_line() +
    labs(title = title, x = '', y = ylab)
  
  if (w_gender) {
    p <- p + scale_color_brewer(palette = 'Dark2', guide = 'none') +
      facet_wrap(~ gender, ncol = 2, scales = 'free_y')
  }
  
  if (!is.null(xintercept)) {
    p <- p + geom_vline(xintercept = xintercept, linetype = 'dashed')
  }
  
  p
}

plot_predictions_prop <- function(pred, y = prop_sumcontribs, title = 'model predictions', ylab = '', xintercept = NULL) {
  y <- enquo(y)
  pred_long <- select(pred, date, observed = !!y, fit) %>%
    gather("type", "value", -date)
  
  p <- ggplot() +
    geom_point(aes(x = date, y = !!y), alpha = 0.5, shape = 21, data = pred) +
    geom_ribbon(aes(x = date, ymin = lwr, ymax = upr), alpha = 0.2, data = pred) +
    geom_line(aes(x = date, y = value, linetype = type), alpha = 0.5, data = pred_long) +
    scale_linetype_discrete(limits = c('observed', 'fit')) +
    labs(title = title, x = '', y = ylab) +
    theme_minimal()
  
  if (!is.null(xintercept)) {
    p <- p + geom_vline(xintercept = xintercept, linetype = 'dashed')
  }
  
  p
}


plot_residuals <- function(pred, y = resid_response, alpha = 1, title = 'model residuals', ylab = '', xintercept = NULL, vertical = TRUE) {
  y <- enquo(y)
  
  if (vertical) {
    facetrows <- 2
    facetcols <- 1
  } else {
    facetrows <- 1
    facetcols <- 2
  }
  
  p <- ggplot(pred, aes(x = date, color = gender)) +
    geom_point(aes(y = !!y), alpha = alpha) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    scale_color_brewer(palette = 'Dark2', guide = 'none') +
    facet_wrap(~ gender, nrow = facetrows, ncol = facetcols, scales = 'free_y') +
    labs(title = title, x = '', y = ylab) +
    theme_minimal()
  
  if (!is.null(xintercept)) {
    p <- p + geom_vline(xintercept = xintercept, linetype = 'dashed')
  }
  
  p
}

plot_residuals_prop <- function(pred, title = 'model residuals', ylab = '', xintercept = NULL) {
  p <- ggplot(pred, aes(x = date)) +
    geom_point(aes(y = resid_response)) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    labs(title = title, x = '', y = ylab) +
    theme_minimal()
  
  if (!is.null(xintercept)) {
    p <- p + geom_vline(xintercept = xintercept, linetype = 'dashed')
  }
  
  p
}


plot_acf_pacf <- function(pred, title = '') {
  wrap_elements(plot = ~acf(filter(pred, gender == 'male') %>% pull(resid_deviance), main = title, sub = 'acf / male')) +
    wrap_elements(plot = ~acf(filter(pred, gender == 'female') %>% pull(resid_deviance), main = '', sub = 'acf / female')) +
    wrap_elements(plot = ~pacf(filter(pred, gender == 'male') %>% pull(resid_deviance), main = '', sub = 'pacf / male')) +
    wrap_elements(plot = ~pacf(filter(pred, gender == 'female') %>% pull(resid_deviance), main = '', sub = 'pacf / female'))
}


plot_acf_pacf_prop <- function(pred, title = '') {
  wrap_elements(plot = ~acf(pred$resid_deviance, main = title, sub = 'acf')) +
    wrap_elements(plot = ~pacf(pred$resid_deviance, main = title, sub = 'pacf'))
}


plot_forecast_gender <- function(plotdata_gender, xintercept, ylab, title = '', startdate = '1990-01-01') {
  filter(plotdata_gender, date >= startdate) %>%
    ggplot(aes(x = date, y = value)) +
    geom_line(aes(color = name, group = name)) +
    geom_vline(xintercept = xintercept, linetype = 'dashed') +
    scale_color_discrete(limits = c('pred', 'actual'),
                         labels = c('pred' = 'predicted', 'actual' = 'actual'),
                         name = '') +
    facet_wrap(~ gender, nrow = 1, scales = 'free_y') +
    labs(title = title, x = '', y = ylab)
}


plot_forecast_prop <- function(plotdata_prop, xintercept, ylab, title = '', startdate = '1990-01-01') {
  filter(plotdata_prop, date >= startdate) %>%
    ggplot(aes(x = date, y = value)) +
    geom_line(aes(color = name)) +
    geom_vline(xintercept = xintercept, linetype = 'dashed') +
    scale_color_discrete(limits = c('pred', 'actual'),
                         labels = c('pred' = 'predicted', 'actual' = 'actual'),
                         name = '') +
    labs(title = title, x = '', y = ylab)
}
