library(quanteda)
library(conText)
library(dplyr)
library(text2vec)
library(readr)
source("utils.R")
set.seed(123L)

# Define the list of countries
countries <-
  c("djazairess", "maghress", "masress", "sauress", "turess")

# get synonym words for old expanded dictionary
# oppterms <- read_csv("data/terms_opp.csv") %>%
#   filter(include == 1)
# oppterms <- oppterms$term
# supterms <- read_csv("data/terms_sup.csv") %>%
#   filter(include == 1)
# supterms <- supterms$term

oppterms <- read_csv("data/pretrained_embedding/oppterms_manual.csv") %>%
  filter(include == 1)
oppterms <- oppterms$term
supterms <- read_csv("data/pretrained_embedding/supterms_manual.csv") %>%
  filter(include == 1)
supterms <- supterms$term

# using opposition minus support vector with multiple words
first_ar = oppterms
second_ar = supterms

# Get cosine similarities
process_cos_sim <-
  function(country_name,
           local_glove,
           local_transform) {
    # Read the modified analysis corpus with "TARGETWORD"
    nws_corpus <-
      readRDS(
        paste0(
          "data/analysis_corpora_gsubbed/",
          country_name,
          "_nws_corpus_leadertarg.rds"
        )
      )
    
    nws_corpus <- corpus(nws_corpus, text_field = "content")
    
    cat("Getting cos_sims for country ", country_name, "\n")
    
    cos_simsdf_all <- get_similarity_scores(x = nws_corpus, 
                                            target = "TARGETWORD",
                                            first_vec = first_ar, 
                                            second_vec = second_ar, 
                                            pre_trained = local_glove,
                                            transform_matrix = local_transform,
                                            window = 12L,
                                            group_var = "yearwk",
                                            norm = "l2")
    
    
    # Return or save results, depending on your needs
    return(cos_simsdf_all)
  }

# Versions to process
sample_sizes <- c(1e4, 5e4, 1e5, 5e5, 1e6, 1.5e6)
sample_sizes <- sapply(sample_sizes, function(x) format(x, scientific = FALSE))
versions <- paste0(sample_sizes, "30k")

# Function to process each version and country
process_version <- function(version) {
  # Read the combined embedding for the specific version
  local_transform <-
    readRDS(paste0(
      "data/embedding_combined/combined_local_transform",
      version,
      ".rds"
    ))
  local_glove <-
    readRDS(paste0(
      "data/embedding_combined/combined_local_glove",
      version,
      ".rds"
    ))
  
  # Loop through countries and apply the process_cos_sim function, saving results
  results <-
    lapply(countries, process_cos_sim, local_glove, local_transform)
  
  # If you need to save the results for each country
  for (i in seq_along(countries)) {
    # Create a subdirectory for the country if it doesn't exist
    dir_name <- paste0("data/output/cos_sims_test/", countries[i])
    if (!dir.exists(dir_name)) {
      dir.create(dir_name)
    }
    
    # Save the result in the country's subdirectory
    saveRDS(results[[i]],
            paste0(dir_name, "/", "cos_simsdf_all", version, ".rds"))
  }
  
}

# Loop through versions and apply the process_version function
lapply(versions, process_version)
