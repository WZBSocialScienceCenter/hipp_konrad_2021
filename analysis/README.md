# Data collection scripts

for paper "Men’s and women’s productivity before and during the COVID-19 pandemic: Evidence from a cross-country comparison of software developers"

March 2021, Lena Hipp <lena.hipp@wzb.eu> and Markus Konrad <markus.konrad@wzb.eu>

This folder holds the R scripts used for data preparation, figure/tables generation and analysis.


## Software requirements

- R 3.6 or newer
- R packages as listed in `renv.lock` -- install them via [renv](https://rstudio.github.io/renv/)


## File overview

### (1) Data preparation scripts

#### (1.1) `aggregdata.R`: Create weekly aggregated GitHub data by country and gender

- uses raw individual GitHub contributions data and user profile data as from Python scripts in the "collection" folder
- generates weekly aggregates per country and gender
- result is written to `aggregdata/contribs_weekly_gender.csv`

#### (1.2) `forecasts.R`: Create forecasts from aggregated GitHub contributions time series data

- takes data from (1.1) and fits a Holt-Winters forecast model per country and gender to predict counterfactual "no-covid" scenario
- predicted and actual values are written to `aggregdata/contribs_weekly_forecast.csv`

#### (1.3) `prepare_macrodata.R`: Prepare several country-specific macrodata sources

- loads and prepares country-specific macrodata (StackOverflow developer survey, COVID-19 Data Hub, CIA factbook, Gender Ineq. Index, GDP)
- result is written to `macrodata` folder

#### (1.4) `forecastdiff_finaldata.R`: Create the final dataset

- takes data from (1.2) and (1.3) and combines all data to form a final dataset
- two variants of the final dataset a produced: a balanced time series dataset (i.e. equal number of weekly obs. per country) and an unbalanced time series dataset
- results are written to `aggregdata/finaldata_balanced.csv` and `aggregdata/finaldata_unbalanced.csv`


### (2) Scripts to generate figures and tables

#### (2.1) `finalplots.R`: Script to generate plots and tables used in the paper

- generates plots and tables used in the paper
- output is saved to `articleplots` and `tables`


### (3) Analysis scripts

#### (3.1) `descriptive_info.R`: Descriptive statistics for GitHub user profile data and contributions data at different levels

- calculate descriptive information used in the paper
- output is stored to `summarydata` folder


### (4) R scripts with helper functions

- `dataprep.R`: Common data preparation functions
- `plot_utils.R`: Plotting utility functions
