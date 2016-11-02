library(tm)
library(SnowballC)
library(magrittr)
library(dplyr)
library(jiebaR)

seg <- worker(stop_word = '/home/r/Scripts/stop_words.utf8',  
              user = '/home/r/Scripts/user.dict.utf8')

doc1 <- "Stray cats are running all over the place. I see 10 a day!"
doc2 <- "Cats are killers. They kill billions of animals a year."
doc3 <- "The best food in Columbus, OH is   the North Market."
doc4 <- "Brand A is the best tasting cat food around. Your cat will love it."
doc5 <- "Buy Brand C cat food for your cat. Brand C makes healthy and happy cats."
doc6 <- "The Arnold Classic came to town this weekend. It reminds us to be healthy."
doc7 <- "I have nothing to say. In summary, I have told you nothing."

query <- "healthy cat food"
chi_query <- '强壮的cat food'

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

chi_doc <- data.frame(
  id = 12, 
  content = enc2utf8('强壮牌猫粮，你的好猫粮，可以让ketty试一试。'), 
  stringsAsFactors = FALSE, row.names = NULL
)

# ===================================== tm 

dfToCorpus <- function(df) {
  myReader <- readTabular(mapping = list(content = 'content', id = 'id'))
  VCorpus(DataframeSource(df), readerControl = list(reader = myReader))
}

corpusToMtx <- function(corpus, seg) {
  # wordSeg <- content_transformer(
  #   function(x) {
  #     # if (!grepl('[\u4e00-\u9fa5]', x)) {
  #     #   return(x)
  #     # }
  #     segment(x, seg)
  #   }
  # )
  
  result <- corpus %>% 
    tm_map(removeNumbers) %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(content_transformer(function(x) {
      segment(x, seg)
    })) %>% 
    tm_map(stemDocument) %>% 
    TermDocumentMatrix(control = list(wordLengths = c(1, Inf))) %>% 
    as.matrix()
  
  return(result)
}

termWeight <- function(tf_vec) {
  result <- rep(0, length(tf_vec))
  result[tf_vec > 0] <- (1 + log2(tf_vec[tf_vec > 0])) * 
    log2(length(tf_vec) / sum(tf_vec > 0))
  return(result)
}

matrixWeightScale <- function(doc, seg) {
  tdmtx <- doc %>% 
    dfToCorpus() %>% 
    corpusToMtx(seg)
  
  mtx_weighted_scaled <- apply(tdmtx, 1, function(x) {
    termWeight(x)
  }) %>% 
    t() %>% 
    set_colnames(colnames(tdmtx)) %>% 
    apply(2, function(x) {
      if (all(x == 0)) {
        return(x)
      }
      scale(x, center = FALSE, scale = sqrt(sum(x ^ 2)))
    }) %>% 
    set_rownames(rownames(tdmtx))
  
  return(mtx_weighted_scaled)
}

newWeightScale <- function(mtx, tdmtx) {
  stopifnot(is.matrix(mtx), 
            ncol(mtx) == 1, 
            is.matrix(tdmtx))
  
  term <- rownames(tdmtx)
  temp <- matrix(0, nrow(tdmtx), ncol(mtx), 
                 dimnames = list(term, colnames(mtx)))
  
  temp[term %in% rownames(mtx), ] <- 
    (1 + log2(mtx[rownames(mtx) %in% term, ])) * 
    log2(ncol(tdmtx) / rowSums(tdmtx[term %in% rownames(mtx), ] > 0))
  
  if (!all(temp == 0)) {
    temp <- scale(temp, center = FALSE, scale = sqrt(sum(temp ^ 2)))
  }
  
  return(temp)
}

newDocCombine <- function(new_doc, tdmtx, seg) {
  
  new_doc_mtx <- new_doc %>% 
    dfToCorpus() %>% 
    corpusToMtx(seg)
  
  result <- newWeightScale(new_doc_mtx, tdmtx) %>% 
    cbind(tdmtx, .)
  
  return(result)
}

querySearch <- function(query, tdmtx, seg, doc, k = 10L, thr = 0) {
  stopifnot(is.numeric(k), 
            is.numeric(thr), 
            is.character(query), 
            is.matrix(tdmtx))
  
  searched <- VectorSource(query) %>% 
    Corpus() %>% 
    corpusToMtx(seg) %>% 
    newWeightScale(tdmtx) %>% 
    crossprod(tdmtx) %>% 
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

# # run daily
# tdmtx <- matrixWeightScale(doc, seg)
# 
# # # run whenever new doc created
# new_tdmtx <- newDocCombine(new_doc, tdmtx, seg) 
# # 
# # run while searching
# querySearch(query, new_tdmtx, seg, rbind(doc, new_doc), 5, 0.1)
# 
# new_tdmtx <- newDocCombine(chi_doc, new_tdmtx, seg)
# 
# querySearch(query, new_tdmtx, seg, rbind(doc, new_doc, chi_doc), 5, 0.1)
# 
# tdmtx <- matrixWeightScale(rbind(doc, new_doc, chi_doc), seg)






