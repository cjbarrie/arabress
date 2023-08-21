library(quanteda)
library(conText)
library(dplyr)
library(text2vec)
library(readr)
library(ggplot2)
source("99_utils.R")
set.seed(123L)

country_name <- "masress"


nws_corpus <-
  readRDS(
    paste0(
      "data/analysis_corpora_gsubbed/",
      country_name,
      "_nws_corpus_leadertarg.rds"
    )
  )

nws_corpus <- nws_corpus %>%
  sample_frac(.1)

nws_corpus <- corpus(nws_corpus, text_field = "content")

# get synonym words for expanded dictionary
oppterms <- read_csv("data/terms_opp.csv") %>%
  filter(include == 1)
oppterms <- oppterms$term
supterms <- read_csv("data/terms_sup.csv") %>%
  filter(include == 1)
supterms <- supterms$term

# using opposition minus support vector with multiple words
first_ar = oppterms
second_ar = supterms

# first_ar = "المعارضة"
# second_ar = "الدعم"

sample_sizes <- c(1e4, 5e4, 1e5, 5e5, 1e6, 1.5e6)
sample_sizes <- sapply(sample_sizes, function(x) format(x, scientific = FALSE))
versions <- paste0(sample_sizes, "30k")
version <- versions[6]

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

cos_simsdf_all <- get_similarity_scores(x = nws_corpus, 
                                        target = "TARGETWORD",
                                        first_vec = first_ar, 
                                        second_vec = second_ar, 
                                        pre_trained = local_glove,
                                        transform_matrix = local_transform,
                                        group_var = "yearwk",
                                        norm = "l2")



result %>%
  mutate(group = as.Date(group)) %>%
  arrange(group) %>%
  rename(yearwk = group,
         cos_sim = val2) %>%
  ggplot(aes(x = yearwk, y = cos_sim)) +
  geom_point(alpha = .25) +
  geom_smooth(
    method = "loess",
    size = 1,
    span = .5,
    fill = "white"
  )






























cos_simsdf_all <- readRDS("/Users/cbarrie6/Dropbox/edbrgh_projects/arabress_medcrit/turess_medcrit/data/output/cos_sims/cos_sims_nws_expanded.rds")

cos_simsdf_all %>%
  arrange(seqvar) %>%
  rename(yearwk = seqvar,
         cos_sim = diff_ar) %>%
  filter(yearwk>="2008-01-01") %>%
  # pivot_longer(!yearwk, names_to = "term", values_to = "cos_sim") %>%
  ggplot() +
  geom_point(aes(yearwk, cos_sim), alpha = .25) +
  geom_smooth(aes(yearwk, cos_sim),
              method = "loess", size = 3,
              fill = "white",
              col = "#8b0000",
              span = .3) +
  theme_tufte(base_family = "Helvetica") +
  labs(x = "Year-week",
       y = "Cosine similarity, expanded opposition index : executive words") +
  geom_vline(aes(xintercept = as.integer(as.Date("2011-02-14"))),
             linetype="longdash", colour = "black", size=.5) +
  geom_vline(aes(xintercept = as.integer(as.Date("2013-07-01"))),
             linetype="longdash", colour = "black", size=.5) +
  scale_color_grey() +
  theme(legend.position = "none",
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        legend.text=element_text(size=20),
        legend.title = element_text(size = 20),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.background = element_rect(fill = "white", colour = NA),
        panel.grid.major = element_line(size = 0.1, linetype = "solid"),
        panel.grid.minor = element_line(size = 0.1, linetype = "solid"),
        strip.text = element_text(size = 20))


library(quanteda)
library(conText)
library(dplyr)
library(text2vec)
source("utils.R")


set.seed(123L)

# Define the list of countries
countries <-
  c("djazairess", "maghress", "masress", "sauress", "turess")

# using opposition minus support vector with single words
first_ar = "المعارضة"
second_ar = "الدعم"


nws_corpus <-
  readRDS(
    paste0(
      "data/analysis_corpora_gsubbed/",
      country_name,
      "_nws_corpus_leadertarg.rds"
    )
  )

# nws_corpus <- nws_corpus %>%
#   sample_frac(.1)

nws_corpus <- corpus(nws_corpus, text_field = "content")
    
    

# Versions to process
sample_sizes <- c(1e4, 5e4, 1e5, 5e5, 1e6, 1.5e6)
sample_sizes <- sapply(sample_sizes, function(x) format(x, scientific = FALSE))
versions <- paste0(sample_sizes, "30k")
version <- versions[6]



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











cos_simsdf_all <- get_similarity_scores(
  x = nws_corpus,
  target = "TARGETWORD",
  first_vec = first_ar,
  second_vec = second_ar,
  pre_trained = local_glove,
  transform_matrix = local_transform,
  group_var = "yearwk",
  norm = "l2"
)

cos_simsdf_all %>%
  mutate(group = as.Date(group)) %>%
  arrange(group) %>%
  rename(yearwk = group,
         cos_sim = val) %>%
  ggplot(aes(x = yearwk, y = cos_sim)) +
  geom_point(alpha = .25) +
  geom_smooth(
    method = "loess",
    size = 1,
    span = .5,
    fill = "white"
  ) +
  labs(x = "Year-week",
       y = "Cosine similarity, leaders : opposition index") + # Label for the color legend
  ylim(-.2, 0.2) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 15),
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
