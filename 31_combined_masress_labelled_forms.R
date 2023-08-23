library(dplyr)
library(tidylog)
library(ggplot2)
library(data.table)
library(quanteda)

# SCRIPT TO COMBINE HUMAN LABELLED DATA

# FORM1
## combine form 1 correct and incorrect responses

files <- list.files("data/output/valtest/masress/qualtrics/completed_forms")
files <- paste0("data/output/valtest/masress/qualtrics/completed_forms/",files)
# get responses to correction form
qual_answers <- fread(files[1])
qual_answers <- qual_answers[,-c(1:17)]
qual_answers <- qual_answers[-c(1:2),]

qual_long <- qual_answers %>% 
  pivot_longer(cols = colnames(qual_answers)[1]:colnames(qual_answers)[ncol(qual_answers)],
               names_to = c("ID")) %>%
  filter(!is.na(value))

qual_long$value <- as.integer(qual_long$value)

qual_long_agg1 <- qual_long %>%
  group_by(ID) %>%
  summarise(score_avg = mean(value))


## get responses to original form

qual_answers <- fread(files[2])
qual_answers <- qual_answers[,-c(1:17)]
qual_answers <- qual_answers[-c(1:7),]

qual_long <- qual_answers %>% 
  pivot_longer(cols = colnames(qual_answers)[1]:colnames(qual_answers)[ncol(qual_answers)],
               names_to = c("ID")) %>%
  filter(!is.na(value))

qual_long$value <- as.integer(qual_long$value)

attention_answers <- qual_long %>%
  filter(grepl("ARTIDCHECK", 
               x = qual_long$ID))
#all correct

qual_long_agg2 <- qual_long %>%
  filter(value !=10) %>% # remove NA values incorrectly coded
  group_by(ID) %>%
  summarise(score_avg = mean(value))

qual_long_agg <- rbind(qual_long_agg1, qual_long_agg2)

saveRDS(qual_long_agg, "data/output/valtest/masress/qualtrics/completed_forms_cleaned/form1.rds")


# FORM2
qual_answers <- fread(files[3])
qual_answers <- qual_answers[,-c(1:17)]
qual_answers <- qual_answers[-c(1:2),]

qual_long <- qual_answers %>% 
  pivot_longer(cols = colnames(qual_answers)[1]:colnames(qual_answers)[ncol(qual_answers)],
               names_to = c("ID")) %>%
  filter(!is.na(value))

qual_long$value <- as.integer(qual_long$value)

attention_answers <- qual_long %>%
  filter(grepl("ARTIDCHECK", 
               x = qual_long$ID))
#all correct

qual_long_agg <- qual_long %>%
  group_by(ID) %>%
  summarise(score_avg = mean(value))

saveRDS(qual_long_agg, "data/output/valtest/masress/qualtrics/completed_forms_cleaned/form2.rds")

# FORM3
qual_answers <- fread(files[4])
qual_answers <- qual_answers[,-c(1:17)]
qual_answers <- qual_answers[-c(1:3),] #First responses removed for this coder as they reported misunderstanding on first attempt

qual_long <- qual_answers %>% 
  pivot_longer(cols = colnames(qual_answers)[1]:colnames(qual_answers)[ncol(qual_answers)],
               names_to = c("ID")) %>%
  filter(!is.na(value))

qual_long$value <- as.integer(qual_long$value)

attention_answers <- qual_long %>%
  filter(grepl("ARTIDCHECK", 
               x = qual_long$ID))
#all correct

qual_long_agg <- qual_long %>%
  group_by(ID) %>%
  summarise(score_avg = mean(value))

saveRDS(qual_long_agg, "data/output/valtest/masress/qualtrics/completed_forms_cleaned/form3.rds")

# FORM4
qual_answers <- fread(files[5])
qual_answers <- qual_answers[,-c(1:17)]
qual_answers <- qual_answers[-c(1:2),]

qual_long <- qual_answers %>% 
  pivot_longer(cols = colnames(qual_answers)[1]:colnames(qual_answers)[ncol(qual_answers)],
               names_to = c("ID")) %>%
  filter(!is.na(value))

qual_long$value <- as.integer(qual_long$value)

attention_answers <- qual_long %>%
  filter(grepl("ARTIDCHECK", 
               x = qual_long$ID))
#all correct

qual_long_agg <- qual_long %>%
  group_by(ID) %>%
  summarise(score_avg = mean(value))

saveRDS(qual_long_agg, "data/output/valtest/masress/qualtrics/completed_forms_cleaned/form4.rds")

# FORM5
## get responses to correction form
qual_answers <- fread(files[6])
qual_answers <- qual_answers[,-c(1:17)]
qual_answers <- qual_answers[-c(1:2),]

qual_long <- qual_answers %>% 
  pivot_longer(cols = colnames(qual_answers)[1]:colnames(qual_answers)[ncol(qual_answers)],
               names_to = c("ID")) %>%
  filter(!is.na(value))

qual_long$value <- as.integer(qual_long$value)

qual_long_agg1 <- qual_long %>%
  group_by(ID) %>%
  summarise(score_avg = mean(value))

## get responses to original form

qual_answers <- fread(files[7])
qual_answers <- qual_answers[,-c(1:17)]
qual_answers <- qual_answers[-c(1:2),]

qual_long <- qual_answers %>% 
  pivot_longer(cols = colnames(qual_answers)[1]:colnames(qual_answers)[ncol(qual_answers)],
               names_to = c("ID")) %>%
  filter(!is.na(value))

qual_long$value <- as.integer(qual_long$value)

attention_answers <- qual_long %>%
  filter(grepl("ARTIDCHECK", 
               x = qual_long$ID))
#all correct

qual_long_agg2 <- qual_long %>%
  filter(value !=10) %>% # remove NA values incorrectly coded
  group_by(ID) %>%
  summarise(score_avg = mean(value))

qual_long_agg <- rbind(qual_long_agg1, qual_long_agg2)

saveRDS(qual_long_agg, "data/output/valtest/masress/qualtrics/completed_forms_cleaned/form5.rds")

# FORM6
qual_answers <- fread(files[8])
qual_answers <- qual_answers[,-c(1:17)]
qual_answers <- qual_answers[-c(1:2),]

qual_long <- qual_answers %>% 
  pivot_longer(cols = colnames(qual_answers)[1]:colnames(qual_answers)[ncol(qual_answers)],
               names_to = c("ID")) %>%
  filter(!is.na(value))

qual_long$value <- as.integer(qual_long$value)

attention_answers <- qual_long %>%
  filter(grepl("ARTIDCHECK", 
               x = qual_long$ID))
#all correct

qual_long_agg <- qual_long %>%
  group_by(ID) %>%
  summarise(score_avg = mean(value))

saveRDS(qual_long_agg, "data/output/valtest/masress/qualtrics/completed_forms_cleaned/form6.rds")

# FORM7

qual_answers <- fread(files[9])
qual_answers <- qual_answers[,-c(1:17)]
qual_answers <- qual_answers[-c(1:2),]

qual_long <- qual_answers %>% 
  pivot_longer(cols = colnames(qual_answers)[1]:colnames(qual_answers)[ncol(qual_answers)],
               names_to = c("ID")) %>%
  filter(!is.na(value))

qual_long$value <- as.integer(qual_long$value)

attention_answers <- qual_long %>%
  filter(grepl("ARTIDCHECK", 
               x = qual_long$ID))
#all correct

qual_long_agg <- qual_long %>%
  group_by(ID) %>%
  summarise(score_avg = mean(value))

saveRDS(qual_long_agg, "data/output/valtest/masress/qualtrics/completed_forms_cleaned/form7.rds")