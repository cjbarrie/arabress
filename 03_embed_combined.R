library(quanteda)
library(text2vec)
library(fst)
library(conText)
library(dplyr)
library(tidylog)
set.seed(123L)

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
total_sample_sizes <- c(1e4, 5e4, 1e5, 5e5, 1e6, 1.5e6)
total_sample_sizes <- sapply(total_sample_sizes, function(x) format(x, scientific = FALSE))

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
total_sample_sizes <- c(1e4, 5e4, 1e5, 5e5, 1e6, 1.5e6)
total_sample_sizes <- sapply(total_sample_sizes, function(x) format(x, scientific = FALSE))

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

# Get transform matrix of varying sizes for 1.5m embedding:
total_size <- total_sample_sizes[6]
weightings <- c(1, 10, 50, 100, 500, 1000, 2000, 10000, 50000, 100000) 
weightings <- sapply(weightings, function(x) format(x, scientific = FALSE))

toks_fcm <- readRDS(file = paste0(destination_path, "combined_fcm", total_size, "30k.rds"))
local_glove <- readRDS(file = paste0(destination_path, "combined_local_glove", total_size, "30k.rds"))

for (weight in weightings) {
  cat("Getting transform matrix for weight ", weight, '\n')
  local_transform <- compute_transform(x = toks_fcm, pre_trained = local_glove, weighting = weight)
  saveRDS(local_transform, file = paste0(destination_path, "combined_local_transform", total_size, "_weight", weight, "30k.rds"))
}

# Get embeddings of varying feature sizes
feature_sizes <- c(1e3, 3e3, 5e3, 10e3, 15e3, 20e3)
feature_sizes <- sapply(feature_sizes, function(x) format(x, scientific = FALSE))
feature_suffixes <- c("1k", "3k", "5k", "10k", "15k", "20k")
names(feature_sizes) <- feature_suffixes

total_sample_size <- c(1.5e6)
total_size <- sapply(total_sample_size, function(x) format(x, scientific = FALSE))

# Read the tokenized corpus for the total_size
toks <- readRDS(file = paste0(destination_path, "combined_toks", total_size, "30k.rds"))
combined_dfm <- dfm(toks, verbose = TRUE)

# Separate loop for feature sizes
for (feat_size in feature_sizes) {
  cat(paste0("Processing feature size ", feat_size, " ...\n"))
  
  top_feats <- featnames(combined_dfm)[order(-colSums(combined_dfm))[1:as.numeric(feat_size)]]
  toks_feats <- tokens_select(toks, top_feats, padding = TRUE)
  
  # Use the named feature size for file naming
  suffix <- names(feature_sizes)[feature_sizes == feat_size]
  saveRDS(toks_feats, file = paste0(destination_path, "combined_toks_feats", total_size, suffix, ".rds"))
  
  # Construct the feature co-occurrence matrix
  toks_fcm <- fcm(
    toks_feats,
    context = "window",
    window = WINDOW_SIZE,
    count = "frequency",
    tri = FALSE,
    weights = rep(1, WINDOW_SIZE)
  )
  saveRDS(toks_fcm, file = paste0(destination_path, "combined_fcm", total_size, suffix, ".rds"))
  
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
  saveRDS(local_glove, file = paste0(destination_path, "combined_local_glove", total_size, suffix, ".rds"))
  
  local_transform <- compute_transform(x = toks_fcm, pre_trained = local_glove, weighting = 100)
  saveRDS(local_transform, file = paste0(destination_path, "combined_local_transform", total_size, suffix, ".rds"))
}