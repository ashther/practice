library(jiebaR)
library(dplyr)
library(magrittr)
library(lda)

# I like to eat broccoli and bananas
# I ate a banana and spinach smoothie for breakfast
# Chinchillas and kittens are cute
# My sister adopted a kitten yesterday
# Look at this cute hamster munching on a piece of broccoli
seg <- worker(bylines = TRUE, stop_word = 'lda/stop_words.utf8')

documents <- readClipboard() %>% 
  segment(seg)
ind <- sapply(documents, function(x)!identical(character(0), x))
documents <- documents[ind]

# words <- unlist(documents) %>% 
#   table() %>% 
#   as.data.frame(stringsAsFactors = FALSE) %>% 
#   set_colnames(c('word', 'freq'))
words <- unlist(documents) %>% 
  table()

wordsIndexFind <- function(x, words) {
  which(x == names(words)) - 1
}

# getTerms <- function(x, words) {
#   result <- which(x %in% words$word) %>% 
#     #which(words$word %in% x) %>%
#     table() %>% 
#     as.data.frame() %>% 
#     t() %>% 
#     as.integer() %>% 
#     matrix(nrow = 2)
#   result[1, ] <- result[1, ] - 1L
#   return(result)
# }
getTerms <- function(x, words) {
  temp <- table(x)
  freq <- unname(temp)
  term <- unname(sapply(names(temp), wordsIndexFind, words = words))
  return(matrix(as.integer(c(term, freq)), nrow = 2, byrow = TRUE))
}

documentsMtx <- lapply(documents, getTerms, words = words)

set.seed(357)
fit <- lda.collapsed.gibbs.sampler(
  documents = documentsMtx, K = 3, vocab = names(words),
  num.iterations = 5000, alpha = 0.1, eta = 0.02,
  initial = NULL, burnin = 0, compute.log.likelihood = TRUE)

theta <- t(apply(fit$document_sums + 0.1, 2, function(x) x/sum(x)))  #文档—主题分布矩阵
phi <- t(apply(t(fit$topics) + 0.02, 2, function(x) x/sum(x)))  #主题-词语分布矩阵
term.frequency <- unname(words)   #词频
doc.length <- sapply(documentsMtx, function(x) sum(x[2, ])) #每篇文章的长度，即有多少个词

require(LDAvis)
json <- createJSON(phi, theta, doc.length, words$word, term.frequency)
serVis(json)


K <- 3
num.iterations <- 10
alpha <- 0.1
eta <- 0.02

termGet <- function(x, K, term.label, term.doc.id) {
  result <- vector(length = K)
  names(result) <- seq_len(K)
  
  term_temp <- table(term.label[term.doc.id == x])
  result[names(term_temp)] <- term_temp
  return(result)
}

thetaCreate <- function(documents, K, alpha, term.label, term.doc.id) {
  documentsMtx <- sapply(seq_along(documents), termGet, K, term.label, term.doc.id) 
  theta <- documentsMtx %>% 
    add(alpha) %>% 
    apply(2, function(x) x/sum(x)) %>% 
    t() %>% 
    set_rownames(seq_along(documents))
  return(theta)
}

phiCreate <- function(documents, K, eta, term.label, voc, term) {
  topicMtx <- matrix(0, nrow = K, ncol = length(voc), 
                     dimnames = list(as.character(seq_len(K)), voc))
  temp <- cbind(term.label, term) %>% 
    as.data.frame(stringsAsFactors = FALSE) %>% 
    group_by(term.label, term) %>% 
    tally()
  topicMtx[as.matrix(temp[, c('term.label', 'term')])] <- temp$n
  phi <- topicMtx %>%  
    add(eta) %>% 
    apply(1, function(x) x/sum(x))
  return(phi)
}

newLabelCreate <- function(documents, theta, phi) {
  term.label.new <- lapply(seq_along(documents), function(x) {
    phi_idx <- rownames(phi) %in% documents[[x]]
    apply(phi[phi_idx, ], 1, function(y)which.max(y * theta[x, ]))
  }) %>% 
    unlist(use.names = FALSE)
  return(term.label.new)
}

lda <- function(documents, K = 3, num.iterations = 10, 
                alpha = 0.1, eta = 0.02, thr = 0.95) {
  cmp <- vector(length = num.iterations)
  term <- unlist(documents, use.names = FALSE)
  term.doc.id <- rep(seq_along(documents), sapply(documents, length))
  voc <- unique(term)
  
  term.label <- sample(K, length(term), replace = TRUE)
  i <- 1
  while (i <= num.iterations) {
    
    theta <- thetaCreate(documents, K, alpha, term.label, term.doc.id)
    phi <- phiCreate(documents, K, eta, term.label, voc, term)
    term.label.new <- newLabelCreate(documents, theta, phi)
    
    cmp[i] <- mean(term.label == term.label.new)
    term.label <- term.label.new
    if (cmp[i] >= thr) {
      break
    }
    i <- i + 1
  }
  return(list(theta = theta, phi = phi, term.label = term.label, cmp = cmp))
}




















