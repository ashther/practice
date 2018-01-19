
library(jiebaR)
seg <- worker(stop_word = 'dict/stop_words.utf8', 
              bylines = TRUE, 
              write = 'NOFILE')
Rcpp::sourceCpp('cosSimCaculateCpp.cpp')

matchModelTrain <- function(news,  
                            n_gram = c(1L, 1L), 
                            term_count_min = 5L, 
                            skip_grams_window = 5L, 
                            wv_size = 50, n_iter = 20) {
  library(text2vec)
  library(magrittr)
  
  cat('itokening scripts...\n')
  it <- itoken(news, progressbar = TRUE)
  rm(news)
  cat('creating vocabulary...\n')
  v <- create_vocabulary(it, ngram = n_gram) %>% 
    prune_vocabulary(term_count_min = term_count_min)
  cat('creating tcm...\n')
  tcm <- create_tcm(it, vocab_vectorizer(v), skip_grams_window = skip_grams_window)
  glove <- GlobalVectors$new(word_vectors_size = wv_size, 
                             vocabulary = v, 
                             x_max = 10)
  wv_main <- glove$fit_transform(tcm, n_iter = n_iter, convergence_tol = 0.001)
  termVec <- wv_main + t(glove$components)
  
  return(termVec)
}

matchModelFileCreate <- function(termVec, dict) {
  library(data.table)
  library(dplyr)
  
  termVecSentMtx <- termVec[rownames(termVec) %in% dict$word, , drop = FALSE]
  temp <- rownames(termVec)
  termVec <- as.data.table(termVec)
  termVec$word <- temp
  rm(temp)
  setkey(termVec, 'word')
  
  termVecSentDT <- data.table(word = rownames(termVecSentMtx))
  dict <- as.data.table(dict)
  termVecSentDT <- dict[termVecSentDT, , on = 'word']
  
  return(list(termVec = termVec,
              termVecSentMtx = termVecSentMtx,
              termVecSentDT = termVecSentDT))
}

matchCaculate <- function(script_dt, thr, dict, termVec, termVecSentMtx) {
  library(data.table)
  script_dt <- as.data.table(script_dt)
  dict <- as.data.table(dict)
  
  words_mtx <- termVec[unique(script_dt$word), , on = 'word', nomatch = 0]
  temp <- words_mtx$word
  words_mtx <- as.matrix(words_mtx[, 1:(ncol(words_mtx) - 1)])
  rownames(words_mtx) <- temp
  rm(temp)
  
  sim_mtx <- cosSimCaculateCpp(words_mtx, termVecSentMtx)
  
  sim_dt <- lapply(seq_len(nrow(sim_mtx)), function(x) {
    word_ori <- rownames(words_mtx)[x]
    value_sim <- sim_mtx[x, ]
    
    idx <- value_sim > thr
    word_sim <- rownames(termVecSentMtx)[idx]
    value_sim <- value_sim[idx]
    
    data.table(word = rep(word_ori, length(word_sim)), 
               word_sim = word_sim, 
               sim = value_sim)
  }) %>% 
    rbindlist()
  result <- dict[
    sim_dt, , on = c(word = 'word_sim'), nomatch = 0
  ][
    , .(i.word, word, sim, ctg)
  ][
    script_dt, , on = c(i.word = 'word'), nomatch = 0
  ]
  setnames(result, c('word', 'word_sim', 'sim', 'ctg', 'id'))
}

script2Table <- function(script, seg) {
  library(jiebaR)
  library(dplyr)
  library(stringr)
  
  temp <- script %>% 
    str_split('\\.|!|\\?|。|；|！|？') %>% 
    unlist() %>% 
    segment(seg)
  # idf <- get_idf(temp)
  # 
  # data_frame(word = unlist(temp), 
  #            id = rep(seq_along(temp), sapply(temp, length))) %>% 
  #   left_join(idf, by = c('word' = 'name')) %>% 
  #   rename('idf' = count)
  data_frame(word = unlist(temp), 
             id = rep(seq_along(temp), sapply(temp, length)))
}

tfIdfCaculate <- function(script_dt) {
  library(data.table)
  script_dt <- as.data.table(script_dt)
  script_dt[, .(tfidf = sum(idf, na.rm = TRUE)), by = .(id, word)]
}

matchModelPredict <- function(script, .seg = seg, thr = 0.5, 
                              .dict = dict, 
                              .termVec = termVec, 
                              .termVecSentMtx = termVecSentMtx) {
  script %>% 
    script2Table(.seg) %>% 
    # tfIdfCaculate() %>% 
    matchCaculate(thr, .dict, .termVec, .termVecSentMtx)
}

# sim <- cosSimCaculateCpp(termVec['特朗普', , drop = FALSE], termVec)
# simDt <- data.table(word = rownames(termVec), sim = sim[1, ])
# simDt[order(sim, decreasing = TRUE), ][1:20]
# 
# sim <- {termVec['特朗普', , drop = FALSE] - termVec['奥巴马', , drop = FALSE] + 
#     termVec['毛泽东', , drop = FALSE]} %>% cosSimCaculateCpp(termVec)
# simDt <- data.table(word = rownames(termVec), sim = sim[1, ])
# simDt[order(sim, decreasing = TRUE), ][1:20]
