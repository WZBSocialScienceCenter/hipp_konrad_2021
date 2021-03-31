#
# Two-way fixed effects models.
#
# March 2021, Lena Hipp <lena.hipp@wzb.eu>, Markus Konrad <markus.konrad@wzb.eu>, Stefan Munnes <stefan.munnes@wzb.eu>
#

library(dplyr)
library(miceadds) #lm.cluster
library(margins)
library(ggplot2)
library(sandwich)
library(lmtest)


#### prepare data ####

data <- read.csv("finaldata_unbalanced.csv") %>%
  rename(country_char  = country_code) %>% 
  mutate(country_code  = as.numeric(as.factor(country_char)),
         closedschools = ifelse(covid_school_closing <= 1, 0, 1),
         curfews       = ifelse(covid_stay_home_restrictions <= 1, 0, 1),
         workplace     = ifelse(covid_workplace_closing <= 1, 0, 1)) %>%
  filter(week <= 27) # <= 28 produce error with data$female == 1


all_country_codes <- sort(unique(data$country_code))
wgt__ <- NULL


#### generate models and calculate margins  ####

# note: this will generate warnings about rank deficient models for some combinations
#       because not all countries have variations in the three policy measure variables,
#       e.g. Norway never had curfews

margins <- lapply(c("closedschools", "curfews", "workplace"), function(var) {
  
  lapply(unique(data$female), function(g) {
    print(paste('variable', var, '; female =', g))
    subsetdata <- filter(data, female == g) %>%
      select(country_code, week, closedschools, curfews, workplace, diff)
    print('> fitting model')
    
    m <- lm.cluster(substitute(diff ~ closedschools + curfews + workplace +
                                  i*factor(country_code) + factor(week), 
                              list(i = as.name(var))),
                    cluster = "country_code",
                    data = subsetdata)
    
    print('> calculating margins')
    
    countries_w_variation <- count(unique(subsetdata[c('country_code', var)]), country_code) %>%
      filter(n > 1) %>% pull(country_code)
    
    m <- margins(m$lm_res, vcov = m$vcov, 
                 variable = var, 
                 at = list(country_code = countries_w_variation))
    
    data.frame(unclass(summary(m)), variable = var, female = g)
  }) %>% 
    bind_rows()
}) %>% 
  bind_rows()


#### generate plot for marginal effects of policy measures per country ####

cntrys_df <- data.frame(country_char = unique(data$country_char),
                        country_code = as.numeric(as.factor(unique(data$country_char))))

cntrys_li <- c("DE", "ES", "GB", "IE", "IT", "NO", "BR", "RU", "IN", "CN", "US", "KR")


coefplot.data <- right_join(margins, cntrys_df, by = 'country_code') %>% 
  filter(country_char %in% cntrys_li) %>% 
  mutate(variable = factor(variable, ordered = T,
                           levels = c("closedschools", "workplace", "curfews"),
                           labels = c("School closings", "Workplace closings", "Curfews")),
         female = factor(female, ordered = F, 
                        levels = c(1, 0), 
                        labels = c("Women", "Men")),
         country_char = factor(country_char, ordered = T, levels = rev(cntrys_li)),
         country_gr = case_when(country_char %in% c("BR", "RU", "IN", "CN")
                                ~ "BRIC\nCountries",
                                country_char %in% c("US", "KR")
                                ~ "Other Rich\nDemocracies",
                                TRUE ~ "European\nCountries"),
         country_gr = factor(country_gr, ordered = T, 
                             levels = c("European\nCountries", 
                                        "BRIC\nCountries",
                                        "Other Rich\nDemocracies")))


coefplot <- ggplot(coefplot.data, aes(x = country_char, y = AME, color = female)) +
  geom_hline(aes(yintercept = 0), alpha = .3) + 
  geom_point(aes(shape = female),
             size = 3, 
             position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0, size = .8) +
  coord_flip() +
  facet_grid(country_gr~variable, scales = "free_y", switch = "y", space = "free_y") + 
  scale_colour_grey(start = .4, end = .7) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.border = element_rect(colour = "Gray"),
        strip.placement = "right",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12),
        legend.text = element_text(size = 10))


ggsave("articleplots/combined_R.png", coefplot)
       #width = 12, height = 20, units = "cm")


#### models M0 to M2 (table 4 in paper) ####

signif_stars <- function(p) {
  ifelse(p <= 0.001, '***', ifelse(p <= 0.01, '**', ifelse(p <= 0.05, '*', '')))
}

# `m` is model output from `lm.cluster`
# `coi` is character vector that specifies coefficients of interest
coeffs_of_interest <- function(m, coi) {
  coeffs <- coeftest(m$lm_res, vcov. = m$vcov)
  which_coeffs <- row.names(coeffs) %in% coi
  coi_values <- coeffs[which_coeffs, 1]
  coi_prt <- coeffs[which_coeffs, 4]
  coi_cis <- coefci(m, coi)
  
  coi_df <- bind_cols(estimand = coi,
                      estimate = coi_values,
                      estimate_p = coi_prt,
                      estimate_signif = signif_stars(coi_prt),
                      as.data.frame(coi_cis))
  row.names(coi_df) <- NULL
  colnames(coi_df) <- c(colnames(coi_df)[1:4], c('lwr', 'upr'))
  coi_df
}

m0_intercepts <- lapply(0:1, function(female) {
  subsetdata <- data[data$female == female,]
  m <- lm.cluster(diff ~ factor(country_code) + factor(week),
                  cluster = 'country_code',
                  data = subsetdata)
  coi_df <- coeffs_of_interest(m, '(Intercept)')
  coi_df$female <- female
  coi_df
}) %>%
  bind_rows() %>%
  mutate(model = 'M0') %>% 
  select(model, female, everything())
m0_intercepts

m1_icept_strngcy <- lapply(0:1, function(female) {
  subsetdata <- data[data$female == female,]
  m <- lm.cluster(diff ~ covid_stringency + factor(country_code) + factor(week),
                  cluster = 'country_code',
                  data = subsetdata)
  coi_df <- coeffs_of_interest(m, c('(Intercept)', 'covid_stringency'))
  coi_df$female <- female
  select(coi_df, female, everything())
}) %>%
  bind_rows() %>%
  mutate(model = 'M1') %>% 
  select(model, female, everything())
m1_icept_strngcy

m2_icept_policies <- lapply(0:1, function(female) {
  subsetdata <- data[data$female == female,]
  m <- lm.cluster(diff ~ closedschools + curfews + workplace + factor(country_code) + factor(week),
                  cluster = 'country_code',
                  data = subsetdata)
  coi_df <- coeffs_of_interest(m, c('(Intercept)', 'closedschools', 'curfews', 'workplace'))
  coi_df$female <- female
  select(coi_df, female, everything())
}) %>%
  bind_rows() %>%
  mutate(model = 'M2') %>% 
  select(model, female, everything())
m2_icept_policies

bind_rows(m0_intercepts, m1_icept_strngcy, m2_icept_policies)
