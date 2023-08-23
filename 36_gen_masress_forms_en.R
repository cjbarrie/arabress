library(tidyverse)
library(quanteda)
library(conText)
library(fst)
library(lubridate)
library(tsibble)
library(zoo)
library(tidylog)
library(ggthemes)
library(quiltr)
library(googleLanguageR)

sports_sample_list <- readRDS("data/output/valtest/masress/sports_sample_list.rds")

## Write Qualtrics forms

files.list <- paste0("data/output/valtest/masress/valtest_samples/", 
                     list.files("data/output/masress/valtest/valtest_samples/"))

#take second ten (first ten are for the original Arabic labeller forms)

files.list <- files.list[11:20]

for (i in 1:length(files.list)) {
  
  nws_sample <- readRDS(files.list[[i]])
  
  sprts_samp <- sports_sample_list[[i]]
  
  nws_sample <- rbind(nws_sample, sprts_samp) # add in attention check sports articles
  
  nws_sample <- nws_sample[sample(1:nrow(nws_sample)), ] # reshuffle
  
  #get article lengths
  artlengths <- sapply(strsplit(nws_sample$content, " "), length)
  nws_sample$artlength <- artlengths
  #filter out articles too long for translate engine API
  nws_sample <- nws_sample[which(nws_sample$artlength <=1000),]
  
  myfile <- file.path(paste0("data/output/valtest/masress/qualtrics/quilted_forms_eng/", 
                             "form", "_0", i+10, ".txt"))
  
  qdat <- quilt_form_data(prompt = "Label this text: ", text = nws_sample$content,
                          response_type = "scale", nlow = 1, nhigh = 10, addID = F)
  
  #translate
  qdat_eng <- gl_translate(
    qdat$prompt,
    target = "en",
    format = c("text"),
    source = ""
  )
  
  qdat <- cbind(qdat, qdat_eng)
  
  #keep equired columns
  qdat <- qdat[,c(3,2)]
  
  #rename columns to original
  colnames(qdat) <- c("prompt", "response_type")
  
  qdat$response_type <- "1 (least critical);2;3;4;5;6;7;8;9 (most critical);NA (not about current President);NA (not about Egypt);NA (other)"
  
  #add article specific IDs
  sample_IDs <- nws_sample$ID
  qdat$id <- sample_IDs #note this has to be lower case "id" to be recognized in quiltr
  
  #NB takes a long time to upload if you use page break every 1 page, hence 20
  quilt_form(input_data = qdat, page_break_every = 20,
             question_type = "singleanswer", 
             filename = myfile)
  
  
}