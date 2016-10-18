library(tm)
library(SnowballC)
library(magrittr)
library(dplyr)

doc1 <- "Stray cats are running all over the place. I see 10 a day!"
doc2 <- "Cats are killers. They kill billions of animals a year."
doc3 <- "The best food in Columbus, OH is   the North Market."
doc4 <- "Brand A is the best tasting cat food around. Your cat will love it."
doc5 <- "Buy Brand C cat food for your cat. Brand C makes healthy and happy cats."
doc6 <- "The Arnold Classic came to town this weekend. It reminds us to be healthy."
doc7 <- "I have nothing to say. In summary, I have told you nothing."

query <- "Healthy cat food"

set.seed(2016)
doc <- data.frame(
  id = sample.int(10, size = 7), 
  content = eval(parse(text = sprintf("c(%s)", paste0('doc', 1:7, collapse = ',')))), 
  stringsAsFactors = FALSE, row.names = NULL
)

new_doc <- data.frame(
  id = 11, 
  content = "A new brand cat food? wow, we got let the ketty have a try the food.", 
  stringsAsFactors = FALSE, row.names = NULL
)

#  data 

dfToCorpus <- function(df) {
  myReader <- readTabular(mapping = list(content = 'content', id = 'id'))
  VCorpus(DataframeSource(df), readerControl = list(reader = myReader))
}

corpusToMtx <- function(corpus) {
  temp <- corpus %>% 
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords, stopwords('english')) %>%
    tm_map(stripWhitespace) %>%
    tm_map(stemDocument)
  
  result <- TermDocumentMatrix(temp) %>% 
    as.matrix()
  
  return(result)
}

termWeight <- function(tf_vec) {
  result <- rep(0, length(tf_vec))
  result[tf_vec > 0] <- (1 + log2(tf_vec[tf_vec > 0])) * 
                        log2(length(tf_vec) / sum(tf_vec > 0))
  return(result)
}

matrixWeightScale <- function(doc) {
  tdmtx <- doc %>% 
    dfToCorpus() %>% 
    corpusToMtx()
  
  mtx_weighted_scaled <- apply(tdmtx, 1, function(x) {
    termWeight(x)
  }) %>% 
    t() %>% 
    set_colnames(colnames(tdmtx)) %>% 
    apply(2, function(x) {
      if (all(x == 0)) {
        return(x)
      }
      scale(x, center = FALSE, scale = sqrt(sum(x^2)))
    })
    
  return(mtx_weighted_scaled)
}

newDocWeightScale <- function(new_doc, tdmtx) {
  
  term <- rownames(tdmtx)
  new_doc_mtx <- new_doc %>% 
    dfToCorpus() %>% 
    corpusToMtx()
  
  temp <- sapply(rownames(tdmtx), function(x) {
    if (!x %in% rownames(new_doc_mtx)) {
      return(0)
    } else {
      (1 + log2(new_doc_mtx[x, ])) * log2(ncol(tdmtx) / sum(tdmtx[x, ] > 0))
    }
  }) %>% 
    as.matrix() %>% 
    set_colnames(new_doc$id) 
  
  if (!all(temp == 0)) {
    temp <- scale(temp, center = FALSE, scale = sqrt(colSums(temp^2)))
  }
    
  return(cbind(tdmtx, temp))
}

querySearch <- function(query, tdmtx, doc, k = 10, thr = 0) {
  stopifnot(is.numeric(k), 
            is.numeric(thr), 
            is.character(query), 
            is.matrix(tdmtx))
  
  term <- rownames(tdmtx)
  
  query_mtx <- VectorSource(query) %>% 
    Corpus() %>% 
    corpusToMtx()
  
  temp <- matrix(0, 1, nrow(tdmtx), dimnames = list('query', term))
  temp[, term %in% rownames(query_mtx)] <- query_mtx[, 1]
  
  searched <- temp %*% tdmtx %>% 
    extract(1, ) %>% 
    Filter(function(x)x >= thr, .)
  
  if (length(searched) == 0) {
    return(NULL)
  }
  
  result <- data.frame(
    id = names(searched)[
      order(searched, decreasing = TRUE)
    ] %>% as.integer()
  ) %>% 
    left_join(doc, 'id')
  
  if (k < 0) {
    return(result$content)
  } 
  
  return(slice(result, seq_len(k))$content)
}

# run daily
tdmtx <- matrixWeightScale(doc)

# # run whenever new doc created
# new_tdmtx <- newDocWeightScale(new_doc, new_tdmtx) 
# 
# # run while searching
# querySearch(query, new_tdmtx, rbind(doc, new_doc), 5, 0.2) 






















