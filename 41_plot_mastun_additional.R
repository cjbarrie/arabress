library(dplyr)
library(tidylog)
library(ggplot2)
library(zoo)
library(tsibble)
library(lubridate)
library(ggthemes)
library(cowplot)

# Egypt

## plot Transformer scores
#get "president corpus" of NER political articles containing president mentions
pres_corpus <- readRDS("data/output/valtest/masress/pres_corpus_ALL.rds")
# make article ID
pres_corpus$ID <- paste0("ART", seq.int(1:nrow(pres_corpus)))

preds <- read.csv("data/output/huggingface/masress/predsdf10k.csv")
kwicdf <- readRDS("data/output/huggingface/masress/fullval10k.rds")
## join on row number

preds_docnames <- cbind(preds, kwicdf)

dates_IDS <- pres_corpus %>%
  select(ID, date)

predsdf_analysis <- preds_docnames %>%
  left_join(dates_IDS, by = "ID") %>%
  select(- X)

predsdf_analysis$critical <- ifelse(predsdf_analysis$label=="LABEL_1", 1,0)
predsdf_analysis$criticalscore <- ifelse(predsdf_analysis$label=="LABEL_1", 
                                         predsdf_analysis$score,0)

g1 <- predsdf_analysis %>%
  mutate(yearmon = as.Date(as.yearmon(date)),
         yearwk = as.Date(yearweek(date)),
         year = year(date),
         art_text = 1) %>%
  group_by(yearwk) %>%
  summarise(sumarts = sum(art_text),
            sumcrit = sum(critical),
            sumcritscore = sum(criticalscore),
            propcrit = sumcrit/sumarts,
            propcritscore = sumcritscore/sumarts) %>%
  ggplot() +
  geom_point(aes(yearwk, propcrit), alpha = .25) +
  geom_smooth(aes(yearwk, propcrit),
              method = "loess", size = 3, fill = "white", span = .3) +
  theme_base() +
  labs(x = "Date",
       y = "BERT Pr(Critical)", title = "Egypt") +
  geom_vline(aes(xintercept = as.integer(as.Date("2013-07-01"))),
             linetype="longdash", colour = "black", size=.5) +
  scale_color_grey() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.1, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
    strip.text = element_text(size = 20)
  )


# Tunisia

#get "president corpus" of NER political articles containing president mentions
pres_corpus <- readRDS("data/output/valtest/turess/pres_corpus_ALL.rds")
# make article ID
pres_corpus$ID <- paste0("ART", seq.int(1:nrow(pres_corpus)))

preds <- read.csv("data/output/huggingface/turess/predsdf.csv")
kwicdf <- readRDS("data/output/huggingface/turess/fullval.rds")
## join on row number

preds_docnames <- cbind(preds, kwicdf)

dates_IDS <- pres_corpus %>%
  select(ID, date)

predsdf_analysis <- preds_docnames %>%
  left_join(dates_IDS, by = "ID") %>%
  select(- X)

predsdf_analysis$critical <- ifelse(predsdf_analysis$label=="LABEL_1", 1,0)
predsdf_analysis$criticalscore <- ifelse(predsdf_analysis$label=="LABEL_1", 
                                         predsdf_analysis$score,0)

g2 <- predsdf_analysis %>%
  mutate(yearmon = as.Date(as.yearmon(date)),
         yearwk = as.Date(yearweek(date)),
         year = year(date),
         art_text = 1) %>%
  group_by(yearwk) %>%
  summarise(sumarts = sum(art_text),
            sumcrit = sum(critical),
            sumcritscore = sum(criticalscore),
            propcrit = sumcrit/sumarts,
            propcritscore = sumcritscore/sumarts) %>%
  ggplot() +
  geom_point(aes(yearwk, propcrit), alpha = .25) +
  geom_smooth(aes(yearwk, propcrit),
              method = "loess", size = 3, fill = "white", span = .3) +
  theme_base() +
  labs(x = "Date",
       y = "BERT Pr(Critical)", title = "Tunisia") +
  scale_color_grey() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.1, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
    strip.text = element_text(size = 20)
  )


## plot GPT-4 scores

tunscores <- readRDS("data/output/valtest/turess/gpt4scores_full.rds")
egyscores <- readRDS("data/output/valtest/masress/gpt4scores_full.rds")

g3 <- egyscores %>%
  ggplot() +
  geom_jitter(aes(date, scoreint)) +
  geom_smooth(aes(date, scoreint),
              method = "loess", size = 3, 
              fill = "white",
              col = "#8b0000") +
  labs(x = "Year", 
       y = "GPT-4 score",title = "") +
  scale_color_grey() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.1, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
    strip.text = element_text(size = 20)
  )

g4 <- tunscores %>%
  ggplot() +
  geom_jitter(aes(date, scoreint)) +
  geom_smooth(aes(date, scoreint),
              method = "loess", size = 3, 
              fill = "white",
              col = "#8b0000") +
  labs(x = "Year", 
       y = "GPT-4 score",title = "") +
  scale_color_grey() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 20),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.1, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
    strip.text = element_text(size = 20)
  )

png(
  "plots/mastun_transgpt.png",
  width = 550,
  height = 300,
  units = 'mm',
  res = 300
)
plot_grid(g1,g3,g2, g4, nrow = 2,
          labels = "AUTO")
dev.off()

# SCRIPT TO PLOT HUMAN LABELLED DATA

files <- list.files("data/output/valtest/masress/qualtrics/completed_forms_cleaned")
files <- paste0("data/output/valtest/masress/qualtrics/completed_forms_cleaned/",files)

all_responses <- data.frame()

for (i in seq_along(files)){
  responses <- readRDS(files[i])
  all_responses <- rbind(all_responses, responses)
}


nwsfiles <- list.files("data/output/valtest/masress/valtest_samples/")
nwsfiles <- paste0("data/output/valtest/masress/valtest_samples/",nwsfiles)

all_nws_samples <- data.frame()
for (i in seq_along(nwsfiles)){
  nws_sample <- readRDS(nwsfiles[i])
  all_nws_samples <- rbind(all_nws_samples, nws_sample)
}

## combine cos_sim scores with labelled scores

daftar_responses <- all_responses %>%
  left_join(all_nws_samples, by = "ID") %>%
  filter(!grepl("ARTIDCHECK",ID)) %>%
  select(ID, score_avg, cos_sim)

saveRDS(daftar_responses, "data/output/valtest/masress/labelled_data.rds")

all_nws_samples$yearmon <- tsibble::yearmonth(all_nws_samples$date)
all_nws_samples$yearwk <- tsibble::yearweek(all_nws_samples$date)

g5 <- all_responses %>%
  left_join(all_nws_samples, by = "ID") %>%
  filter(score_avg<10) %>% # remove NAs
  group_by(yearwk) %>%
  summarise(score_avg = mean(score_avg),
            cos_sim = mean(cos_sim)) %>%
  ggplot() +
  geom_jitter(aes(cos_sim, score_avg), height = 1.5) +
  geom_smooth(aes(cos_sim, score_avg), method = "lm")  +
  theme_tufte(base_family = "Helvetica") +
  labs(x = "Mean year-week cosine similarity score",
       y = "Mean year-week scores by human labellers") + 
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.1, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
    strip.text = element_text(size = 20)
  ) 

## testing over-time trends

g6 <- all_responses %>%
  left_join(all_nws_samples, by = "ID") %>%
  filter(score_avg<10) %>% # remove NAs 
  select(ID, score_avg, cos_sim, yearwk) %>%
  group_by(yearwk) %>%
  summarise(score_avg = mean(score_avg)) %>%
  ggplot() +
  geom_jitter(aes(as.Date(yearwk), score_avg)) +
  geom_smooth(aes(as.Date(yearwk), score_avg), method = "loess") +
  theme_tufte(base_family = "Helvetica") +
  labs(x = "Year-week",
       y = "Average article scores by human labellers") + 
  theme(
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      size = 1
    ),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(size = 0.1, linetype = "solid"),
    panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
    strip.text = element_text(size = 20)
  )

png(
  "plots/masress_human_val.png",
  width = 550,
  height = 300,
  units = 'mm',
  res = 300
)
plot_grid(g5,g6, nrow = 1,
          labels = "AUTO")
dev.off()