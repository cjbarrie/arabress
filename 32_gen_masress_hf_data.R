library(dplyr)
library(tidylog)

# SCRIPT TO GENERATE HUGGINGACE TEST TRAIN DATA FOR FINE TUNING

nwsfiles <- list.files("data/output/valtest/masress/valtest_samples/")
nwsfiles <- paste0("data/output/valtest/masress/valtest_samples/",nwsfiles)

all_nws_samples <- data.frame()
for (i in seq_along(nwsfiles)){
  nws_sample <- readRDS(nwsfiles[i])
  all_nws_samples <- rbind(all_nws_samples, nws_sample)
}

#outputting for huggingface

test <- all_nws_samples %>%
  select(ID, content) %>%
  left_join(daftar_responses, by = "ID") %>%
  filter(!is.na(score_avg)) %>%
  select(content, score_avg, cos_sim, ID)

test$scoreint <- as.integer(test$score_avg)
test$scorefac <- ifelse(test$scoreint>5 & test$scoreint<10, "critical",
                        ifelse(test$scoreint==5, "neutral",
                               ifelse(test$scoreint<5,"uncritical", "not-applicable")))


#reshuffle data to randomize coders between training and test
set.seed(123L)
test <- test[sample(1:nrow(test)), ]

target_ar <- "الرئيس"
test$docname <- paste0("text", as.character(seq(1:nrow(test))))

# get kwics with 30 word window
testkwic <- quanteda::kwic(quanteda::tokens(test$content), pattern = target_ar, 
                           valuetype = "regex", window = 30)

testkwic$fullstring <- paste(testkwic$pre, testkwic$pattern, testkwic$post)

test <- test %>%
  select(docname, scorefac)

testkwic <- as.data.frame(testkwic)

testkwic <- testkwic %>%
  select(fullstring, docname)

kwicdf <- testkwic %>%
  left_join(test, by = "docname") %>%
  select(fullstring, scorefac)

kwicdf$label <- ifelse(kwicdf$scorefac=="critical", 1,
                       ifelse(kwicdf$scorefac=="neutral", 0, 
                              ifelse(kwicdf$scorefac=="uncritical", 2, 3)))

kwicdf <- kwicdf[,c(1,3)]
colnames(kwicdf) <- c("text", "label")

testdat <- kwicdf[1:4810,]
traindat <- kwicdf[4811:14430,]
write.csv(testdat, "data/output/huggingface/masress/kwicwords30/test.csv", row.names = F)
write.csv(traindat, "data/output/huggingface/masress/kwicwords30/train.csv", row.names = F)