# Data collection scripts

for paper *Men’s and women’s productivity before and during the COVID-19 pandemic: Evidence from a cross-country comparison of software developers*.

April 2021, Markus Konrad <markus.konrad@wzb.eu> and Lena Hipp <lena.hipp@wzb.eu>

## Software requirements

- the scripts were implemented with Python 3.8, but should run on other versions of Python 3, too 
- use a Python package manager like *pip* with the `requirements.txt` file to install all required packages 

## Sampling and data collection

Sampling: We use a combination of the [GitHub REST API v3](https://developer.github.com/v3/) and web scraping to obtain our primary data, which is GitHub user profile data and [user contributions to GitHub repositories](https://help.github.com/en/github/setting-up-and-managing-your-github-profile/why-are-my-contributions-not-showing-up-on-my-profile). Simply listing all users via the REST API and then taking a random sample is not possible, since you cannot list all of the 50M+ user accounts. The API doesn't provide random sampling and the results for all user listing and search queries are always sorted according to certain criteria, e.g. "popularity" (the default) or account creation date and you will only get the first 1000 results. Hence we do the following to obtain a random sample of users with varying account age:

- generate 100 random dates between the site's launch date and May 15, 2020;
- for each of these dates, search for users that created their account at this day;
- since it is possible that more than 1000 users are in the sorted result for a given day, perform the search with sorting according to different criteria (account creation time at this day and number of followers) and in ascending and descending order
- with this approach we cannot sample enough user accounts from smaller countries, hence we also use search queries for specific countries; since many users don't specify their country in the GitHub profile "location" field but their city, we additionally search for the biggest cities in these countries (using "data/city_searches.csv")

We only retain user profile data with a possibly valid "name" field (i.e. there must be at least the possibility of a fore- and lastname). 

We use several Python scripts and web services to:

- (1) sample GitHub user account creation dates used for the described sampling strategy
- (2) fetch user profile data from GitHub that were created at the sampled dates
- (3) prepare user profile data for further processing
- (4) fetch each user's historical contribution data from GitHub
- (5) geocode user locations (if given)
- (6) predict gender from names
- (7) perform final data preparation
- (8) concatenate datasets from multiple runs to single dataset

The details to these steps are explained in the next section.

### Data collection scripts

**Note on API keys:** We use three web services to obtain the data: GitHub, Google Maps and genderize.io. You must provide an API key for each of these web services in a file `accountdata.py`:

```python
GITHUB_ACCESS_TOKEN = '...'
GOOGLE_MAPS_API_KEY = '...'
GENDERIZE_API_KEY = '...'
```

#### (1) `sampledates.py`: sample GitHub user account creation dates

As explained in the sampling strategy: generate 100 random dates between the site's launch date and May 15, 2020.

- Input: None
- Output: `data/sampled_dates_*.csv`

#### (2) `getusers.py`: fetch user profile data from GitHub that were created at the sampled dates 

Obtain user profile data from GitHub as explained in the sampling strategy. This script used to also fetch contributions data via [GitHub GraphQL API v4](https://developer.github.com/v4/), but since this API only returns data for the last 365 days, we chose to separately obtain this data via web scraping as outlined in step (4). 

- Input: sampled dates from (1) listed in `data/sampled_dates_*.csv` 
- Output: `data/collected_users_*.pickle` (raw user profile data in pickle format)

#### (3) `prepare_collected_data.py`: prepare user profile data for further processing

- Input: raw user profile data from (2) contained in `data/collected_users_*.pickle`
- Output: `data/users_*.csv` (prepared user data)

#### (4) `scrape_contribs.py`: fetch each user's historical contribution data from GitHub

Obtain daily contribution counts per user for the years 2014 to 2020. This is done via web scraping since the GitHub API only returns data for the last 365 days.

The obtained data can be linked with the user profile data via `id` field. 

- Input: prepared user profile data from (3) contained in `users_*.csv`
- Output: `data/contribs_scraped_*.csv` and `data/contribs_scraped_*.pickle` with daily contribution counts per user 

#### (5) `geocode_locations.py`: geocode user locations (if given)

Try to obtain geographic information such as geolocation coordinates and country from the `location` field for each user profile via [Google Maps Geocoding API](https://cloud.google.com/maps-platform/).

The obtained data can be linked with the user profile data via `location` field. 

- Input: prepared user profile data from (3) contained in `users_*.csv`
- Output: `data/geocoded_locations_*.pickle`)

#### (6) `identify_gender.py`: predict gender from names

Try to identify the gender for each user profile from the given name in the `name` field by using the [genderize.io](https://genderize.io/) API. If location data from (5) is given for a user account, make an additional query using also the country code for country-specific naming conventions. Also, in some (specifically Eastern Asian) countries it is common to put the given name last. For these countries, also query the API with the last name.

The obtained data can be linked with the user profile data via `id` field. 

- Input:
  - prepared user profile data from (3) contained in `users_*.csv`
  - geolocation data from (5) contained in file `geocoded_locations_*.pickle`
- Output: `data/users_genderizer_results_*.pickle`

#### (7) `finalize_data.py`: perform final data preparation

Take intermediate raw data from (3), (5) and (6), preprocess and join it to arrive at a final user profile dataset. For the gender prediction data, only set the gender of a user profile when the best prediction has at least 80% probability from at least 100 entries in the genderize.io database. 

- Input:
  - prepared user profile data from (3) contained in `users_*.csv`
  - geolocation data from (5) contained in file `geocoded_locations_*.pickle`
  - gender prediction results data from (6) contained in file `users_genderizer_results_*.pickle` 
- Output: `data/usersfull_*.csv`

#### (8) `concat_final_data.py`: concatenate datasets from multiple runs to single dataset

Load all files matching the pattern `data/usersfull*.csv` and concatenate them to a single user profiles dataset named `concat_users.csv`. Load all files matching the pattern `data/contribs_scraped*.pickle` and concatenate them to a single user contributions dataset named `concat_contribs.csv`.

- Input:
    - `usersfull*.csv` files from different runs of (7)
    - `contribs_scraped*.pickle` files from different runs of (4)
- Output: `data/concat_users.csv` and `data/concat_contribs.csv`
