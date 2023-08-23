Sys.setenv(openai_organization_id = {"ORG_ID_HERE"})
Sys.setenv(openai_secret_key = {"SECRETKEY_HERE"})

library(openai)
library(purrr)
library(tidylog)
library(dplyr)
library(ggplot2)

# Egypt: have to take data from the survey forms as initially generated in .txt qualtrics format
set.seed(123L)

modelist <- list_models() %>% 
  pluck('data') %>% 
  map_dfr(compact)

test <- readLines("data/output/valtest/masress/qualtrics/quilted_forms_eng/forms_all_en.txt")

test <- paste(test, collapse=' ')

articles <- stringr::str_split(test, "\\[\\[ID:")

artdf <- as.data.frame(articles)

colnames(artdf) <- "text"

artdf$ID <- stringr::str_extract(artdf$text, "[^]]+")
artdf <- artdf[-1,]

artdf$text_cleaned <- sub(".*Label this text: ", "", artdf$text)

artdf$nchar <- nchar(artdf$text)

artdfsamp <- artdf %>%
  filter(nchar <=4000) %>%
  sample_n(1000)

artdfscores <- data.frame(text = NA,
                          ID = NA, 
                          score = NA)


for (i in 1:nrow(artdfsamp)) {
  
  text <- artdfsamp[i,3]
  ID <- artdfsamp[i,2]
  
  cat("Scoring article ", i, " of ", nrow(artdfsamp), "\n")
  
  output <- create_chat_completion(
    model = "gpt-4",
    max_tokens = 1,
    temperature = .2,
    n = 1,
    stream = F, 
    messages = list(
      list(
        "role" = "system",
        "content" = "You are coding some news media text for whether or not it is critical toward the figure of the President."
      ),
      list(
        "role" = "user",
        "content" = paste("On a scale of 1-9, where 9 represents very critical and 1 represents not critical at all, how critical is this news text toward the President of Egypt:\n", text)
      )
    )
  )
  
  score <- output[["choices"]][[1]][["message"]][["content"]]
  
  artdfrow <- as.data.frame(cbind(text, ID, score))
  artdfscores <- bind_rows(artdfscores, artdfrow)
  
}

# saveRDS(artdfscores, "data/output/valtest/gpt35scores.rds")
#1 USD for 1k articles for GPT-3.5 but garbage output

# saveRDS(artdfscores, "data/output/valtest/gpt4scores.rds")
#11 USD for 1k articles for GPT-4

files.list <- paste0("data/output/valtest/masress/valtest_samples/", 
                     list.files("data/output/valtest/masress/valtest_samples/"))

#take second ten (first ten are for the original Arabic labeller forms)

files.list <- files.list[11:20]

nws_samples_all <- data.frame()
for (i in 1:length(files.list)) {
  
  nws_sample <- readRDS(files.list[[i]])
  
  nws_samples_all <- rbind(nws_samples_all, nws_sample) # add in attention check sports articles
  
}

nws_sample <- nws_samples_all %>%
  select(date, ID)

testjoin <- artdfscores %>%
  left_join(nws_sample, by = "ID") %>%
  filter(!is.na(date))

testjoin$scoreint <- as.integer(testjoin$score)

saveRDS(testjoin, "data/output/valtest/masress/gpt4scores_full.rds")
