library(dplyr)
library(lubridate)
library(tsibble)
library(zoo)

# Define the list of countries
countries <- c("djazairess", "maghress", "masress", "sauress", "turess")

output_path <- "data/output/combined/"
analysis_corpora_path <- "data/analysis_corpora/"

# Loop through all countries
for (country in countries) {
  cat(paste0("Processing ", country, " ...\n"))
  
  # Read in newspaper corpus for each country
  nws_corpus <- readRDS(paste0(output_path, country,"/", country, "_combined_prepro.rds"))
  
  # format date
  nws_corpus$date <- as.character(nws_corpus$date)
  nws_corpus$date <- as.Date(nws_corpus$date, format = "%Y%m%d")
  
  # gen year-month, year-week, and year variables
  nws_corpus <- nws_corpus %>%
    mutate(yearmon = as.Date(as.yearmon(date)),
           yearwk = as.Date(yearweek(date)),
           year = year(date))
  
  # keep only articles after 2008
  nws_corpus <- nws_corpus %>%
    filter(date>="2008-01-01")
  
  # Save news corpus for the current country
  saveRDS(nws_corpus, file = paste0(analysis_corpora_path, country, "_nws_corpus.rds"))
}