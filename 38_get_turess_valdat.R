library(dplyr)
library(tidylog)
library(ggplot2)
library(fst)
library(zoo)
library(tsibble)
library(lubridate)
library(ggthemes)
library(stringr)

pres_corpus <- readRDS("data/output/valtest/turess/pres_corpus_ALL.rds")

# make article ID
pres_corpus$ID <- paste0("ART", seq.int(1:nrow(pres_corpus)))

# SCRIPT TO GENERATE DATA TO BE CLASSIFIED BY FINE TUNED HUGGINFACE MODEL

#filter out articles already used for labelling
set.seed(123L)
pres_corpus_filtered <- pres_corpus %>%
  select(content, ID) 

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

write.table(kwicdf, "data/output/huggingface/turess/fullval.txt", row.names = F, col.names = F, sep = ";")
## UPLOAD TO HUGGINGFACE FOR CLASSIFICATION







## Generate English-language forms for GPT-4
library(googleLanguageR)
# set key
# usethis::edit_r_environ(): auth file is set to another sandbox location for now (change this in prod.)

pres_corpus <- readRDS("data/output/valtest/turess/pres_corpus_ALL.rds")

# make article ID
pres_corpus$ID <- paste0("ART", seq.int(1:nrow(pres_corpus)))

# SCRIPT TO GENERATE DATA TO BE CLASSIFIED BY FINE TUNED HUGGINFACE MODEL

#filter out articles already used for labelling
set.seed(123L)
pres_corpus_filtered <- pres_corpus %>%
  select(content, ID) 

pres_corpus_sample <- pres_corpus_filtered %>%
  sample_n(2000)

#get article lengths
artlengths <- sapply(strsplit(pres_corpus_sample$content, " "), length)
pres_corpus_sample$artlength <- artlengths
#filter out articles too long for translate engine API
pres_corpus_sample <- pres_corpus_sample[which(pres_corpus_sample$artlength <=1000),]

pres_corpus_sample <- pres_corpus_sample%>%
  sample_n(1000)

pres_corpus_sample$trans <- gl_translate(
  pres_corpus_sample$content,
  target = "en",
  format = c("text"),
  source = ""
)

pres_corpus_sample$translation

saveRDS(pres_corpus_sample,"data/output/valtest/turess/pres_corpus_sample_trans.rds")

