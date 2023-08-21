# Old function
# get_seq_cos_sim <- function (x, seqvar, target, candidates, pre_trained, transform_matrix, norm,
#                              window = 6, valuetype = "fixed", case_insensitive = TRUE, 
#                              hard_cut = FALSE, verbose = TRUE) 
# {
#   veclist <- list()
#   seqvals <- unique(seqvar)
#   for (i in seq_along(seqvals)) {
#     seqval = seqvals[[i]]
#     contextftu <- tryCatch({
#       get_context(x = x[which(seqvar == seqval)], 
#                   target = target, window = window, valuetype = valuetype, 
#                   case_insensitive = case_insensitive, hard_cut = hard_cut, 
#                   verbose = verbose)
#     }, error = function(e) {
#       warning(paste("Did not find any context for sequence variable", seqval, ", replaced with NULL"), call. = FALSE)
#       data.frame(docname = NA, target = NA, context = NA)
#     })
#     
#     error <- tryCatch(veclist[[i]] <- embed_target(context = contextftu$context, 
#                                                    pre_trained, transform_matrix, transform = TRUE, 
#                                                    aggregate = TRUE, verbose = verbose), error = function(e) e)
#     if (inherits(error, "error")) {
#       veclist[[i]] <- list(target_embedding = NULL, local_vocab = NULL, 
#                            obs_included = NULL)
#     }
#   }
#   cos_simsdf <- data.frame()
#   cos_sims <- vector()
#   for (i in seq_along(veclist)) {
#     target_embedding = veclist[[i]][["target_embedding"]]
#     if (is.null(target_embedding)) {
#       cos_sim <- as.vector(rep(NA, length(candidates)))
#     }
#     else {
#       cos_sim <- find_cos_sim(target_embedding = target_embedding, 
#                               pre_trained, candidates = candidates, norm = norm) # change here to allow for inner product
#     }
#     cos_sim <- as.vector(cos_sim)
#     cos_sims <- rbind(cos_sims, cos_sim)
#     cos_simsdf <- as.data.frame(cos_sims, row.names = F)
#   }
#   for (i in seq_along(candidates)) {
#     cname = candidates[[i]]
#     names(cos_simsdf)[i] <- paste0(cname)
#   }
#   cos_simsdf <- cbind(cos_simsdf, seqvals)
#   names(cos_simsdf)[names(cos_simsdf) == "seqvals"] <- "seqvar"
#   return(cos_simsdf)
# }
# 
# 
# 
# find_cos_sim <- function(target_embedding,
#                          pre_trained,
#                          candidates,
#                          norm = norm)
# {
#   if (length(candidates) == 1){
# 
#       # Adjust the sim2 call to utilize norm
#     # This workaround is needed because norm is set to "l2" in the sim2 function of text2vec
#     error <- tryCatch(
#       cos_sim <- text2vec::sim2(
#         x = matrix(pre_trained[candidates, ], nrow = 1),
#         y = matrix(target_embedding, nrow = 1),
#         method = "cosine",
#         norm = norm
#       ),
#       error = function(e) e
#     )
#     if (inherits(error, 'error')) {
#       warning("No instances of candidate: '",
#               candidates,
#               "' found in pre-trained embeddings")
#       cos_sim <- as.vector(rep(NA, 1))
#       return(cos_sim)
#     }
#     row.names(cos_sim) <- candidates
#     return(cos_sim)}else{
#       
#       if (length(candidates) > 1)
#         cos_sim <- vector()
#       for (i in seq_along(candidates)) {
#         candidate = candidates[[i]]
#         error <-
#           tryCatch(
#             cos_sim_i <- text2vec::sim2(
#               x = matrix(pre_trained[candidate, ], nrow = 1),
#               y = matrix(target_embedding, nrow = 1),
#               method = "cosine",
#               norm = norm
#             ),
#             error = function(e)
#               e
#           )
#         if (inherits(error, 'error')) {
#           warning("No instances of candidate: '",
#                   candidate,
#                   "' found in pre-trained embeddings")
#           cos_sim_i <- as.vector(rep(NA, 1))
#         }
#         cos_sim <- rbind(cos_sim, cos_sim_i)
#       }
#       
#       row.names(cos_sim) <- candidates
#       return(cos_sim)}
# }

get_similarity_scores <- function(x, 
                                  target = "TARGETWORD", 
                                  first_vec, 
                                  second_vec, 
                                  pre_trained, 
                                  transform_matrix,
                                  group_var,
                                  window = window,
                                  norm = "l2",
                                  remove_punct = FALSE, 
                                  remove_symbols = FALSE, 
                                  remove_numbers = FALSE, 
                                  remove_separators = FALSE,
                                  valuetype = "fixed",
                                  hard_cut = FALSE,
                                  case_insensitive = TRUE) {
  
  # Tokenize corpus
  toks <- tokens(x, remove_punct = remove_punct, remove_symbols = remove_symbols, 
                 remove_numbers = remove_numbers, remove_separators = remove_separators)
  
  # Build tokenized corpus of contexts surrounding the target word
  target_toks <- tokens_context(x = toks, pattern = target, 
                                valuetype = valuetype, window = window, 
                                hard_cut = hard_cut, case_insensitive = case_insensitive)
  
  # Compute ALC embeddings
  target_dfm <- dfm(target_toks)
  target_dem <- dem(x = target_dfm, pre_trained = pre_trained, 
                    transform = TRUE, transform_matrix = transform_matrix, 
                    verbose = TRUE)
  
  # Aggregate embeddings over the grouping variable
  target_dem_grouped <- dem_group(target_dem, 
                                  groups = target_dem@docvars[[group_var]]) 
  
  # Check if vector is of length >1 to determine if necessary to transpose
  if (length(first_vec) > 1) {
    y_matrix = as.matrix(pre_trained[intersect(first_vec, rownames(pre_trained)),])
  } else {
    y_matrix = t(as.matrix(pre_trained[intersect(first_vec, rownames(pre_trained)),]))
  }
  
  # Cosine similarity for first vector of terms
  group_first_val <- sim2(target_dem_grouped, 
                          y = y_matrix, 
                          method = 'cosine', norm = norm)
  
  group_first_val <- rowMeans(group_first_val) #TODO not necessary if first/second vec of length 1
  group_first_val <- tibble(group = factor(names(group_first_val)), 
                            first_val = unname(group_first_val))
  
  # Check if vector is of length >1 to determine if necessary to transpose
  if (length(second_vec) > 1) {
    y_matrix = as.matrix(pre_trained[intersect(second_vec, rownames(pre_trained)),])
  } else {
    y_matrix = t(as.matrix(pre_trained[intersect(second_vec, rownames(pre_trained)),]))
  }
  
  # Cosine similarity for negative second vector of terms
  group_sec_val <- sim2(target_dem_grouped, 
                          y = y_matrix, 
                          method = 'cosine', norm = norm)
  
  group_sec_val <- rowMeans(group_sec_val) #TODO not necessary if first/second vec of length 1
  group_sec_val <- tibble(group = factor(names(group_sec_val)), 
                          sec_val = unname(group_sec_val))
  
  result <- left_join(group_first_val, group_sec_val, by = "group") %>% 
    mutate(val = (1)*first_val + (-1)*sec_val) %>%
    select(group, val)
  
  return(result)
}
