Sys.setenv(openai_organization_id = {"org-DGX068z0rNFGth6i2zEmnici"})
Sys.setenv(openai_secret_key = {"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"})

library(openai)
library(purrr)
library(tidylog)
library(dplyr)
library(ggplot2)

set.seed(123L)

modelist <- list_models() %>% 
  pluck('data') %>% 
  map_dfr(compact)

# Tunisia: can take data translated and stored in tidy .rds format
pres_corpus_sample <- readRDS("data/output/valtest/turess/pres_corpus_sample_trans.rds")

pres_corpus_sample$text <- pres_corpus_sample$trans$translatedText

artdf <- pres_corpus_sample %>%
  select(text, ID)

artdf$nchar <- nchar(artdf$text)

artdfsamp <- artdf %>%
  filter(nchar <=4000)


artdfscores <- data.frame(text = NA,
                          ID = NA, 
                          score = NA)


for (i in 830:nrow(artdfsamp)) {
  
  text <- artdfsamp[i,1]
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
        "content" = paste("On a scale of 1-9, where 9 represents very critical and 1 represents not critical at all, how critical is this news text toward the President of Tunisia:\n", text)
      )
    )
  )
  
  score <- output[["choices"]][[1]][["message"]][["content"]]
  
  artdfrow <- as.data.frame(cbind(text, ID, score))
  artdfscores <- bind_rows(artdfscores, artdfrow)
  
}

saveRDS(artdfscores, "data/output/valtest/turess/gpt4scores.rds")
#18USD for 1k articles for GPT-4

pres_corpus <- readRDS("data/output/valtest/turess/pres_corpus_ALL.rds")
# make article ID
pres_corpus$ID <- paste0("ART", seq.int(1:nrow(pres_corpus)))

nws_sample <- pres_corpus %>%
  select(date, ID)

testjoin <- artdfscores %>%
  left_join(nws_sample, by = "ID") %>%
  filter(!is.na(date))

testjoin$scoreint <- as.integer(testjoin$score)

saveRDS(testjoin, "data/output/valtest/turess/gpt4scores_full.rds")
