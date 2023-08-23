library(quanteda)
library(conText)
library(dplyr)
library(text2vec)
source("utils.R")

set.seed(123L)

# Define the list of countries
countries <-
  c("masress", "turess")

# using opposition minus support vector with single words
first_ar = "المعارضة"
second_ar = "الدعم"

# Read the combined embedding for the biggest version
local_transform <-
  readRDS(paste0(
    "data/embedding_combined/combined_local_transform150000030k.rds"
  ))
local_glove <-
  readRDS(paste0(
    "data/embedding_combined/combined_local_glove150000030k.rds"
  ))


# Modify process_cos_sim to filter by newspaper
process_cos_sim <- function(country_name, newspaper_name, local_glove, local_transform) {
  
  nws_corpus <- read_corpus_by_source(country_name)
  nws_corpus <- nws_corpus[nws_corpus$newspaper == newspaper_name, ]
  nws_corpus <- corpus(nws_corpus, text_field = "content")
  
  cat("Getting cos_sims for country ", country_name, " and newspaper ", newspaper_name, "\n")
  
  cos_simsdf_all <- get_similarity_scores(
    x = nws_corpus, 
    target = "TARGETWORD",
    first_vec = first_ar, 
    second_vec = second_ar, 
    pre_trained = local_glove,
    transform_matrix = local_transform,
    window = 12L,
    group_var = "yearwk",
    norm = "l2"
  )
  cos_simsdf_all$newspaper <- newspaper_name
  cos_simsdf_all$country <- country_name
  
  return(cos_simsdf_all)
}

# Function to read in the corpus based on the source name
read_corpus_by_source <- function(source_name) {
  readRDS(
    paste0(
      "data/analysis_corpora_gsubbed/",
      source_name,
      "_nws_corpus_leadertarg.rds"
    )
  )
}

# Read in the masress and turess corpora
masress_corpus <- read_corpus_by_source("masress")
masress_corpus$country <- "masress"
turess_corpus <- read_corpus_by_source("turess")
turess_corpus$country <- "turess"

# Count the articles by source and arrange in descending order
masress_sources <- masress_corpus %>%
  group_by(newspaper) %>%
  count() %>%
  arrange(desc(n))

turess_sources <- turess_corpus %>%
  group_by(newspaper) %>%
  count() %>%
  arrange(desc(n))

# Get top 10 newspapers for each country
top10_masress <- head(masress_sources$newspaper, 10)
top10_turess <- head(turess_sources$newspaper, 10)

# Main loop to process each country and top 10 newspapers
for (country in countries) {
  # Determine the top 10 newspapers based on the country
  if (country == "masress") {
    top_newspapers <- top10_masress
  } else if (country == "turess") {
    top_newspapers <- top10_turess
  }  
  # List to store the results for each newspaper in the country
  all_newspapers_results <- list()
  
  for (newspaper in top_newspapers) {
    # Process the cos_sim for the country-newspaper pair
    result <- process_cos_sim(country, newspaper, local_glove, local_transform)
    
    # Store the result in the list
    all_newspapers_results[[newspaper]] <- result
  }
  
  # Combine all newspaper scores together
  combined_results <- dplyr::bind_rows(all_newspapers_results, .id = "newspaper")
  
  # Save the combined results for the entire country
  dir_name <- paste0("data/output/cos_sims_mastun/")
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
  }
  saveRDS(combined_results, paste0(dir_name, "/cos_simsdf_all_bysource.rds"))
}