library(dplyr)
library(tidylog)
library(ggplot2)
library(fst)
library(zoo)
library(tsibble)
library(lubridate)
library(ggthemes)

# SCRIPT TO GENERATE DATA TO BE CLASSIFIED BY FINE TUNED HUGGINFACE MODEL

#get news samples used for Daftar labelling
nwsfiles <- list.files("data/output/valtest/masress/valtest_samples/")
nwsfiles <- paste0("data/output/valtest/masress/valtest_samples/",nwsfiles)
nwsfiles <- nwsfiles[1:10]


all_nws_samples <- data.frame()

for (i in seq_along(nwsfiles)){
  nws_sample <- readRDS(nwsfiles[i])
  all_nws_samples <- rbind(all_nws_samples, nws_sample)
}

IDs <- unique(all_nws_samples$ID)

#get "president corpus" of NER political articles containing president mentions
pres_corpus <- readRDS("data/output/valtest/masress/pres_corpus_ALL.rds")
# make article ID
pres_corpus$ID <- paste0("ART", seq.int(1:nrow(pres_corpus)))

#filter out articles already used for labelling
set.seed(123L)
pres_corpus_filtered <- pres_corpus %>%
  filter(!ID %in% IDs) %>%
  select(content, ID) %>%
  sample_n(100000)

target_ar <- "الرئيس"
pres_corpus_filtered$docname <- paste0("text", as.character(seq(1:nrow(pres_corpus_filtered))))
docnames <- pres_corpus_filtered %>%
  select(docname, ID)

# get kwics with 30 word window
testkwic <- quanteda::kwic(quanteda::tokens(pres_corpus_filtered$content), pattern = target_ar, 
                           valuetype = "regex", window = 30)

testkwic$fullstring <- paste(testkwic$pre, testkwic$pattern, testkwic$post)

testkwic <- as.data.frame(testkwic)

testkwic <- testkwic %>%
  select(fullstring, docname)

kwicdf <- testkwic %>%
  left_join(docnames, by = "docname")

colnames(kwicdf) <- c("text", "docname", "ID")

write.table(kwicdf, "data/output/huggingface/masress/fullval10k.txt", row.names = F, col.names = F, sep = ";")
saveRDS(kwicdf, "data/output/huggingface/masress/fullval10k.rds")

## SEND TO HUGGINGFACE FOR CLASSIFICATION