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
# oppterms <- read_csv("data/terms_opp.csv") %>%
#   filter(include == 1)
# oppterms <- oppterms$term
# supterms <- read_csv("data/terms_sup.csv") %>%
#   filter(include == 1)
# supterms <- supterms$term

# using opposition minus support vector with multiple words
# first_ar = oppterms
# second_ar = supterms

first_ar = "المعارضة"
second_ar = "الدعم"

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


x = nws_corpus;
target = "TARGETWORD";
first_vec = first_ar;
second_vec = second_ar; 
pre_trained = local_glove;
transform_matrix = local_transform;
group_var = "yearwk";
norm = "l2";
remove_punct = FALSE;
remove_symbols = FALSE; 
remove_numbers = FALSE; 
remove_separators = FALSE;
valuetype = "fixed";
window = 6L;
hard_cut = FALSE;
case_insensitive = TRUE



cos_simsdf_all <- get_similarity_scores(x = nws_corpus, 
                                        target = "TARGETWORD",
                                        first_vec = first_ar, 
                                        second_vec = second_ar, 
                                        pre_trained = local_glove,
                                        transform_matrix = local_transform,
                                        group_var = "yearwk",
                                        norm = "none")



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
  )







#############################










country_name <- "masress"


library(quanteda)
library(conText)
library(dplyr)
library(text2vec)
library(readr)
library(ggplot2)
source("99_utils.R")
set.seed(123L)

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



x = nws_corpus;
target = "TARGETWORD";


pre_trained = local_glove;
transform_matrix = local_transform;

remove_punct = FALSE;
remove_symbols = FALSE; 
remove_numbers = FALSE; 
remove_separators = FALSE;
valuetype = "fixed";
window = 6L;
hard_cut = FALSE;
case_insensitive = TRUE



# using opposition minus support vector with single words
first_ar = "المعارضة"
second_ar = "الدعم"

# get vector of opposition and support words from pre-trained local embedding layer
firstvec_ar = matrix(local_glove[first_ar, ], nrow = 1)
secondvec_ar = matrix(local_glove[second_ar, ], nrow = 1)

# subtract support from opposition to give opposition axis
diff_ar = firstvec_ar - secondvec_ar

# add into local glove matrix
local_glovep <- rbind(local_glove, diff_ar)
v <- rownames(local_glovep) # get rownames
v[nrow(local_glovep)] <- "diff_ar" # name diff_ar
rownames(local_glovep) <- v # name diff_ar

# get vector of year-weeks needed as seqvar for get_seq_cos_sim()
yearwk <- nws_corpus$yearwk

x = nws_corpus$content
seqvar = yearwk
target = "TARGETWORD"
candidates = "diff_ar"
pre_trained = local_glovep
transform_matrix = local_transform

cos_simsdf_all <-
  get_seq_cos_sim(
    x = nws_corpus$content,
    seqvar = yearwk,
    target = "TARGETWORD",
    candidates = "diff_ar",
    pre_trained = local_glovep,
    transform_matrix = local_transform
  )