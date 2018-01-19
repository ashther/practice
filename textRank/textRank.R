

seg <- jiebaR::worker(stop_word = 'dict/stop_words.utf8', bylines = TRUE, write = 'NOFILE')

sentenceSegment <- function(text, delimiters) {
  require(stringr)
  require(magrittr)
  text %>% 
    str_split(delimiters) %>% 
    unlist() %>% 
    `[`(!str_detect(., '^$') & nchar(str_trim(.)) > 0) %>% 
    unique() %>% 
    str_trim() %>% 
    str_replace_all('[:space:]', '')
}

jacSimCaculate <- function(x, y) {
  if (length(x) == 0 | length(y) == 0) {
    return(0)
  }
  inter_length <- max(sum(x %in% y), sum(y %in% x))
  if (inter_length == 0) {
    return(0)
  }
  denominator <- log(length(x)) + log(length(y))
  if (denominator < 1e-12) {
    return(0)
  }
  return(inter_length / denominator)
}

simMtxCreate <- function(sentences) {
  simMtx <- matrix(0, length(sentences), length(sentences))
  invisible(lapply(seq_len(nrow(simMtx)), function(x) {
    invisible(lapply(1:x, function(y) {
      simMtx[x, y] <<- jacSimCaculate(sentences[[x]], sentences[[y]])
    }))
  }))
  return(simMtx)
}

simMtxWECreate <- function(sentences, termVec) {
  sentences <- lapply(sentences, function(x) {
    idx <- x[x %in% rownames(termVec)]
    tryCatch({
      if (length(idx) == 1) {
        termVec[idx, ]
      } else {
        colMeans(termVec[idx, ])
      }
    }, 
      error = function(e) {print(idx);stop()}
    )
  })
  simMtx <- matrix(0, length(sentences), length(sentences))
  invisible(lapply(seq_len(nrow(simMtx)), function(x) {
    invisible(lapply(1:x, function(y) {
      simMtx[x, y] <<- cosSimCaculateCpp(matrix(sentences[[x]], nrow = 1), 
                                         matrix(sentences[[y]], nrow = 1))
    }))
  }))
  simMtx[is.nan(simMtx)] <- 0
  return(simMtx)
}

textRankCaculate <- function(simMtx, damping = 0.85) {
  require(igraph)
  net <- graph_from_adjacency_matrix(simMtx, mode = 'lower', weighted = TRUE, diag = FALSE)
  page_rank(net, damping = damping, directed = FALSE)$vector
  # evcent(net)$vector
}

sentenceTopGet <- function(sentences, text_rank, k = 1) {
  k <- ifelse(length(sentences) >= k, k, length(sentences))
  ord <- order(text_rank, decreasing = TRUE)
  sentences[ord[1:k]]
}
Rcpp::sourceCpp('jacSimCaculateCpp.cpp')

textRank4Sentence <- function(text, k = 5, .seg = seg, damping = 0.85) {
  delimiters <- '[?!;？！。；…\n]'
  sent <- sentenceSegment(text, delimiters)
  sentences <- jiebaR::segment(sent, .seg)
  
  # unlist(lapply(seq_len(k), function(x) {
  #   temp <- sentences[(length(sentences)/k * (x - 1) + 1):(length(sentences)/k * x)]
  #   simMtx <- simMtxCreateCpp(temp)
  #   text_rank <- textRankCaculate(simMtx, damping = damping)
  #   sentenceTopGet(sent, text_rank, k = 1)
  # }))
  
  simMtxCreateCpp(sentences) %>% 
    textRankCaculate(damping = damping) %>% 
    sentenceTopGet(sent, ., k = k) %>% 
    print()
  
  cat('===\n')
  
  script <- scriptCreate(text, seg)
  termVec <- termVecTrain(script$seg_words, word_vectors_size = 100)
  simMtxWECreate(sentences, termVec) %>% 
    textRankCaculate(damping = damping) %>% 
    sentenceTopGet(sent, ., k = k)
}

# lapply(list.files('e:/work/sentiment_glove/data/scripts/', full.names = TRUE),
#        function(x) {
#          print(basename(x))
#          textRank4Sentence(readRDS(x), delimiters = delimiters, seg = seg, k = 10)
#        })
