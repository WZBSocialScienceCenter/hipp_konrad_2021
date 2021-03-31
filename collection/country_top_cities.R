# Generate a list of search queries by obtaining the largest three cities by population per country.
#
# August 2020, Markus Konrad <markus.konrad@wzb.eu>


library(rnaturalearth)
library(dplyr)

CACHE_DIR <- 'cache/ne'

#setwd('..')

#ne_download(scale = 10, type = 'populated_places', destdir = CACHE_DIR)
pop <- ne_load(scale = 10, type = 'populated_places', destdir = CACHE_DIR, returnclass = 'sf') %>%
  sf::st_drop_geometry()

countries <- read.csv('data/countries_few_obs.csv', stringsAsFactors = FALSE)

topcities <- select(pop, NAME, NAMEPAR, NAMEASCII, ISO_A2, POP_MIN) %>%
  filter(ISO_A2 %in% countries$country_code) %>%
  group_by(ISO_A2) %>%
  top_n(3, POP_MIN) %>%
  ungroup() %>%
  arrange(ISO_A2, desc(POP_MIN))

stopifnot(all(countries$country_code %in% topcities$ISO_A2))

count(topcities, ISO_A2) %>% filter(n < 3)   # only singapore

queries <- sort(unique(c(countries$country_name, topcities$NAME, topcities$NAMEASCII, topcities$NAMEPAR[!is.na(topcities$NAMEPAR)])))
queries_df <- data.frame(query = tolower(queries),
                         stringsAsFactors = FALSE)
queries_df$is_city <- !(queries_df$query %in% tolower(countries$country_name))
queries_df

write.csv(queries_df, 'data/city_searches.csv', row.names = FALSE)
