library(quanteda)
library(text2vec)
library(fst)
library(conText)
library(dplyr)
library(tidylog)
set.seed(123L)

#TODO atm. have only done 1m and 1.5m samples. Need still to do the others

# Define the list of countries
countries <-
  c("djazairess", "maghress", "masress", "sauress", "turess")

# Script parameters
WINDOW_SIZE <- 6
DIM <- 300
ITERS <- 200

origin_path <- "data/output/combined/"
# Number of countries
num_countries <- length(countries)

# Define the total sample sizes
total_sample_sizes <- c(1e4, 5e4, 1e5, 5e5)
total_sample_sizes <- sapply(total_sample_sizes, function(x) format(x, scientific = FALSE))
# total_sample_sizes <- c(1e6, 1.5e6)

# Destination path
destination_path <- "data/embedding_combined/"

# Loop through total sample sizes
for (total_size in total_sample_sizes) {
  country_comb_all <- data.frame()
  
  # Calculate the sample size per country
  size_per_country <- total_size / num_countries
  
  # Loop through all countries
  for (country in countries) {
    cat(paste0("Processing ", country, " for total size ", total_size, " ...\n"))
    
    # Read the preprocessed .rds file for the current country
    country_comb <- readRDS(paste0("data/output/combined/", country, "/", country, "_combined_prepro.rds"))
    
    # Sample the current size per country
    country_sample <- country_comb %>%
      sample_n(min(nrow(country_comb), size_per_country))
    
    # Combine the samples
    country_comb_all <- rbind(country_comb_all, country_sample)
  }
  
  # Save the combined sample
  saveRDS(country_comb_all, file = paste0(destination_path, "country_comb_all_", total_size, ".rds"))
}

# List of total sample sizes
total_sample_sizes <- c(1e4, 5e4, 1e5, 5e5)
total_sample_sizes <- sapply(total_sample_sizes, function(x) format(x, scientific = FALSE))
# total_sample_sizes <- c(1e6, 1.5e6)

# Destination path
destination_path <- "data/embedding_combined/"

# Iterate through total sample sizes
for (total_size in total_sample_sizes) {
  cat(paste0("Processing total size ", total_size, " ...\n"))
  
  # Read the combined sample for the specific size
  country_comb_all <- readRDS(file = paste0(destination_path, "country_comb_all_", total_size, ".rds"))
  
  # Tokenize corpus
  country_comb_corpus <- corpus(country_comb_all, text_field = "content")
  toks <- tokens(country_comb_corpus)
  saveRDS(toks, file = paste0(destination_path, "combined_toks", total_size, "30k.rds"))
  
  # Get top k features
  combined_dfm <- dfm(toks, verbose = TRUE)
  top_feats <- featnames(combined_dfm)[order(-colSums(combined_dfm))[1:30000]]
  
  # leave the pads so that non-adjacent words will not become adjacent
  toks_feats <- tokens_select(toks, top_feats, padding = TRUE)
  saveRDS(toks_feats, file = paste0(destination_path, "combined_toks_feats", total_size, "30k.rds"))
  
  # Construct the feature co-occurrence matrix
  toks_fcm <- fcm(
    toks_feats,
    context = "window",
    window = WINDOW_SIZE,
    count = "frequency",
    tri = FALSE,
    weights = rep(1, WINDOW_SIZE)
  )
  saveRDS(toks_fcm, file = paste0(destination_path, "combined_fcm", total_size, "30k.rds"))
  
  # Estimate GloVe model using text2vec
  glove <- GlobalVectors$new(rank = DIM, x_max = 100, learning_rate = 0.05)
  wv_main <- glove$fit_transform(
    toks_fcm,
    n_iter = ITERS,
    convergence_tol = 1e-3,
    n_threads = parallel::detectCores()
  )
  wv_context <- glove$components
  local_glove <- wv_main + t(wv_context)
  saveRDS(local_glove, file = paste0(destination_path, "combined_local_glove", total_size, "30k.rds"))
  
  local_transform <- compute_transform(x = toks_fcm, pre_trained = local_glove, weighting = 100)
  saveRDS(local_transform, file = paste0(destination_path, "combined_local_transform", total_size, "30k.rds"))
}
