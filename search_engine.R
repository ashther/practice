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
  wordSeg <- content_transformer(
    function(x) {
      # if (!grepl('[\u4e00-\u9fa5]', x)) {
      #   return(x)
      # }
      segment(x, seg)
    }
  )
  
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

# ===================================== use text2vec package
library(text2vec)

stop_words <- c("i", "me", "my", "myself", "we", "our", "ours",
                "ourselves", "you", "your", "yours")

pre_func <- func

it <- itoken(
  doc$content,
  preprocessor = tolower,
  tokenizer = word_tokenizer,
  ids = doc$id
)

vcb <- create_vocabulary(it,
                         ngram = c(1, 2),
                         stopwords = stop_words) %>%
  prune_vocabulary(term_count_min = 1)
vcb_vectorizer <- vocab_vectorizer(vcb)

dtm <- create_dtm(it, vcb_vectorizer)

# ===================================== benchmark

microbenchmark::microbenchmark(
  tm = {
    VCorpus(DataframeSource(doc), readerControl = list(reader = myReader)) %>% 
      tm_map(content_transformer(tolower)) %>% 
      DocumentTermMatrix()
  }, 
  
  text2vec = {
    it <- itoken(
      doc$content,
      preprocessor = tolower,
      tokenizer = word_tokenizer,
      ids = doc$id, 
      progressbar = FALSE
    )
    
    vcb <- create_vocabulary(it) 
    vcb_vectorizer <- vocab_vectorizer(vcb)
    create_dtm(it, vcb_vectorizer)
  }
)

# Unit: milliseconds
#      expr        min         lq      mean     median         uq        max neval
#        tm   4.118634   4.279641   4.56663   4.562923   4.770023   5.467505   100
#  text2vec 166.909831 168.211648 170.69139 169.954393 171.375765 194.208945   100

library(Rcpp)
sourceCpp('mtxMultCPP.cpp')
sourceCpp('mtxMultArmCPP.cpp')
sourceCpp('mtxMultEigenCPP.cpp')
sourceCpp('mtxMultParCPP.cpp')

set.seed(2016)
temp <- matrix(1e3, 1, 1e3)
t_temp <- t(temp)
test <- matrix(1e8, 1e3, 1e5)

microbenchmark::microbenchmark(
  # mtxMultEigen(temp, test),
  mtxMultCPP(temp, test),
  # mtxMultArm(temp, test),
  mtxMultParCPP(temp, test),
  # temp %*% test,
  crossprod(t_temp, test), 
  times = 100)

# Unit: milliseconds
#                       expr      min       lq     mean   median       uq       max neval
#     mtxMultCPP(temp, test) 92.32242 93.35680 94.61640 94.45920 95.45292  99.63526   100
#  mtxMultParCPP(temp, test) 67.34526 67.66152 68.34254 68.04931 68.67653  71.57881   100
#    crossprod(t_temp, test) 92.30385 93.55285 94.72991 94.56995 95.49165 100.31737   100







