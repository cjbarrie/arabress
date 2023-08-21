library(dplyr)
library(fst)
library(conText)
library(data.table)
library(dplyr)
library(readr)
source("utils.R")
set.seed(123L)

# Define the list of countries
countries <-
  c("djazairess", "maghress", "masress", "sauress", "turess")

# Function to replace leader names with TARGETWORD in the corpus
process_leaders <- function(country_name) {
  # Read the corresponding analysis corpus
  nws_corpus <-
    readRDS(paste0("data/analysis_corpora/", country_name, "_nws_corpus.rds"))
  
  nws_corpus <- nws_corpus %>%
    sample_n(5e5)
  
  # Filter the leaders for the specific country
  leaders_df <-
    read.csv("data/leaders.csv", stringsAsFactors = FALSE) %>%
    filter(country == country_name)
  
  # Converting the start and end dates to Date format
  leaders_df$start <- as.Date(leaders_df$start, format = "%d/%m/%Y")
  leaders_df$end <- as.Date(leaders_df$end, format = "%d/%m/%Y")
  
  # Looping through each row in the news data frame
  for (i in 1:nrow(nws_corpus)) {
    # Get the article's date
    article_date <- nws_corpus$date[i]
    # Looping through each row in the leaders data frame
    for (j in 1:nrow(leaders_df)) {
      # Check if the article's date is within the start and end dates for the leader
      if (article_date >= leaders_df$start[j] &&
          article_date <= leaders_df$end[j]) {
        # Replace the leader's name with "TARGETWORD" in the content column
        nws_corpus$content[i] <-
          gsub(leaders_df$name[j], "TARGETWORD", nws_corpus$content[i])
        leader_name <- leaders_df$name[j]
        nws_corpus$leader_name[i] <- leader_name
      }
    }
  }
  
  # Save the modified corpus to the analysis_corpora folder
  saveRDS(
    nws_corpus,
    paste0(
      "data/analysis_corpora_gsubbed/",
      country_name,
      "_nws_corpus_leadertarg.rds"
    )
  )
}

# Loop through countries and apply the process_leaders function
lapply(countries, process_leaders)


# Function to replace leader names with TARGETWORD in the corpus
process_exec <- function(country_name) {
  # Read the corresponding analysis corpus
  nws_corpus <-
    readRDS(paste0("data/analysis_corpora/", country_name, "_nws_corpus.rds"))
  
  nws_corpus <- nws_corpus %>%
    sample_n(5e5)
  
  # Filter the leaders for the specific country
  exec_df <-
    read.csv("data/executive.csv", stringsAsFactors = FALSE) %>%
    filter(country == country_name)
  
  # Looping through each row in the news data frame
  for (i in 1:nrow(nws_corpus)) {
    # Looping through each row in the leaders data frame
    for (j in 1:nrow(exec_df)) {
      # Replace the leader's name with "TARGETWORD" in the content column
      nws_corpus$content[i] <-
        gsub(exec_df$name[j], "TARGETWORD", nws_corpus$content[i])
    }
  }
  
  # Save the modified corpus to the analysis_corpora folder
  saveRDS(
    nws_corpus,
    paste0(
      "data/analysis_corpora_gsubbed/",
      country_name,
      "_nws_corpus_exectarg.rds"
    )
  )
}

# Loop through countries and apply the process_leaders function
lapply(countries, process_exec)





# # Get cosine similarities
# process_cos_sim <-
#   function(country_name,
#            local_glove,
#            local_transform) {
#     # Read the modified analysis corpus with "TARGETWORD"
#     nws_corpus <-
#       readRDS(
#         paste0(
#           "data/analysis_corpora_gsubbed/",
#           country_name,
#           "_nws_corpus_leadertarg.rds"
#         )
#       )
#     
#     # using opposition minus support vector with single words
#     first_ar = "المعارضة"
#     second_ar = "الدعم"
#     
#     # get vector of opposition and support words from pre-trained local embedding layer
#     firstvec_ar = matrix(local_glove[first_ar,], nrow = 1)
#     secondvec_ar = matrix(local_glove[second_ar,], nrow = 1)
#     
#     # subtract support from opposition to give opposition axis
#     diff_ar = firstvec_ar - secondvec_ar
#     
#     # add into local glove matrix
#     local_glovep <- rbind(local_glove, diff_ar)
#     v <- rownames(local_glovep) # get rownames
#     v[nrow(local_glovep)] <- "diff_ar" # name diff_ar
#     rownames(local_glovep) <- v # name diff_ar
#     
#     # get vector of year-weeks needed as seqvar for get_seq_cos_sim()
#     yearwk <- nws_corpus$yearwk
#     
#     cat("Getting cos_sims for country ", country_name, "\n")
#     
#     cos_simsdf_all <-
#       get_seq_cos_sim(
#         x = nws_corpus$content,
#         seqvar = yearwk,
#         target = "TARGETWORD",
#         candidates = "diff_ar",
#         pre_trained = local_glovep,
#         transform_matrix = local_transform,
#         norm = "l2"
#       )
#     
#     # Return or save results, depending on your needs
#     return(cos_simsdf_all)
#   }
# 
# # Versions to process
# sample_sizes <- c(1e4, 5e4, 1e5, 5e5, 1e6, 1.5e6)
# sample_sizes <- sapply(sample_sizes, function(x) format(x, scientific = FALSE))
# versions <- paste0(sample_sizes, "30k")
# 
# # Function to process each version and country
# process_version <- function(version) {
#   # Read the combined embedding for the specific version
#   local_transform <-
#     readRDS(paste0(
#       "data/embedding_combined/combined_local_transform",
#       version,
#       ".rds"
#     ))
#   local_glove <-
#     readRDS(paste0(
#       "data/embedding_combined/combined_local_glove",
#       version,
#       ".rds"
#     ))
#   
#   # Loop through countries and apply the process_cos_sim function, saving results
#   results <-
#     lapply(countries, process_cos_sim, local_glove, local_transform)
#   
#   # If you need to save the results for each country
#   for (i in seq_along(countries)) {
#     # Create a subdirectory for the country if it doesn't exist
#     dir_name <- paste0("data/output/cos_sims/", countries[i])
#     if (!dir.exists(dir_name)) {
#       dir.create(dir_name)
#     }
#     
#     # Save the result in the country's subdirectory
#     saveRDS(results[[i]],
#             paste0(dir_name, "/", "cos_simsdf_all", version, ".rds"))
#   }
#   
# }
# 
# # Loop through versions and apply the process_version function
# lapply(versions, process_version)