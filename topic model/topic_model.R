
# predict ----------------------------------------------------------------

# transform doc_embeddings or doc_topic_distr to genre prop
deToGenre <- function(new_doc_embeddings, genre_distr) {
  library(dplyr)
  library(magrittr)
  
  this <- as.matrix(select(genre_distr, -name, -distr))
  if (is.vector(new_doc_embeddings) && !is.list(new_doc_embeddings)) {
    this <- this %*% new_doc_embeddings
  } else {
    this <- this %*% t(as.matrix(new_doc_embeddings))
  }
  this <- apply(this, 2, prop.table)
  
  lapply(seq_len(ncol(this)), function(x) {
    ratio <- this[, x] / genre_distr$distr
    temp <- mutate(genre_distr, prop = as.numeric(scale(ratio) * distr))
    temp <- filter(temp, prop >= 0)
    temp <- mutate(temp, 
                   prop = round(prop.table(prop), 3), 
                   cumsum = cumsum(prop))
    temp <- arrange(temp, desc(prop))
    select(temp, name, prop, cumsum)
  })
}

# apply models in modelLdaList to scripts_pred, get genre distribution
topicModelPredict <- function(scripts_pred, modelLdaList, vectorizer) {
  library(dplyr)
  library(magrittr)
  library(text2vec)
  
  futile.logger::flog.threshold(futile.logger::ERROR)
  
  scripts_pred %>% 
    itoken(progressbar = FALSE) %>% 
    create_dtm(vectorizer, type = 'dgTMatrix') %>% {
      dtm <- .
      lapply(modelLdaList, function(m) {
        de <- m$model_lda$transform(
          dtm, 
          # n_iter = 100, 
          # n_check_convergence = 101, # for prevent flog.info
          progressbar = FALSE
        )
        deToGenre(de, m$gd)
      })
    } %>% 
    do.call(bind_rows, .) %>% 
    group_by(name) %>% 
    summarise(prop = sum(prop)) %>% 
    arrange(desc(prop)) %>% 
    mutate(prop = prop.table(prop), 
           cumsum = cumsum(prop))
}

# train new modelLdaList -------------------------------------------------

# genre-topic spread table, for re-train use
genreTopicSpreadCreate <- function(doc_topic_distr, genre) {
  library(tibble)
  library(dplyr)
  
  # get genre count dataframe list for each topic
  ctg <- apply(doc_topic_distr, 1, which.max)
  ctg <- setNames(lapply(unique(ctg), function(x) {
    temp <- genre$genre[genre$name %in% names(ctg)[ctg == x]]
    enframe(table(temp))
  }), unique(ctg))
  
  # rbind genre count dataframe list to dataframe
  nm <- names(ctg)
  ctg <- Reduce(function(x, y)full_join(x, y, by = 'name'), ctg)
  colnames(ctg)[-1] <- nm
  ctg <- left_join(data_frame(name = unique(genre$genre)), 
                   ctg, by = 'name')
  ctg <- mutate_at(ctg, vars(-name), funs({.[is.na(.)] <- 0;as.integer(.)}))
  
  # maybe genre_distr didn't have the same topic number which n_topic indicate
  na_cols <- setdiff(seq_len(ncol(doc_topic_distr)), colnames(ctg[, -1]))
  if (length(na_cols) > 0) {
    ctg <- cbind(
      ctg, 
      matrix(0, nrow(ctg), length(na_cols), dimnames = list(NULL, na_cols))
    )
  }
  ctg <- ctg[, c(1, order(as.integer(colnames(ctg)[-1])) + 1)]
  ctg <- mutate(ctg, distr = prop.table(rowSums(ctg[, -1], na.rm = TRUE)))
  
  return(ctg)
}

# this when you need re-train to get new modelLdaList --------------------
# data_train is a list which has elements as segment words vector
topicModelTrain <- function(data_train, genre, n_gram, 
                            n_topic, doc_topic_prior, topic_word_prior) {
  library(text2vec)
  
  it <- itoken(data_train, progressbar = FALSE)
  cat('creating vocabulary...\n')
  v <- create_vocabulary(it, ngram = n_gram) %>%
    prune_vocabulary(
      term_count_max = length(data_train) * 3,
      term_count_min = 5, 
      # doc_proportion_min = 0.1,
      doc_proportion_max = 0.4
    )
  vectorizer <- vocab_vectorizer(v)
  
  cat('creating dtm...\n')
  dtm_lda_train <- create_dtm(it, vectorizer, type = 'dgTMatrix')
  
  cat('training the model list...\n')
  modelLdaList <- pbapply::pblapply(1:10, function(x) {
    tf_idf <- TfIdf$new()
    model_lda <- LDA$new(n_topic = n_topic,
                         # vocabulary = v,
                         doc_topic_prior = doc_topic_prior,
                         topic_word_prior = topic_word_prior)
    
    doc_topic_distr <- model_lda$fit_transform(dtm_lda_train,
                                               n_iter = 1000,
                                               convergence_tol = 0.001,
                                               check_convergence_every_n = 25,
                                               progressbar = FALSE)
    
    genre_distr <- genreTopicSpreadCreate(doc_topic_distr, genre)
    return(list(tf_idf = tf_idf, model_lda = model_lda,
                dt = doc_topic_distr, gd = genre_distr))
  })
  
  return(list(modelLdaList = modelLdaList, vectorizer = vectorizer))
}
