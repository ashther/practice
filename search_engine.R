library(tm)
library(SnowballC)
library(magrittr)

doc1 <- "Stray cats are running all over the place. I see 10 a day!"
doc2 <- "Cats are killers. They kill billions of animals a year."
doc3 <- "The best food in Columbus, OH is   the North Market."
doc4 <- "Brand A is the best tasting cat food around. Your cat will love it."
doc5 <- "Buy Brand C cat food for your cat. Brand C makes healthy and happy cats."
doc6 <- "The Arnold Classic came to town this weekend. It reminds us to be healthy."
doc7 <- "I have nothing to say. In summary, I have told you nothing."

doc_list <- eval(parse(text = sprintf("list(%s)", paste0('doc', 1:7, collapse = ','))))
N_docs <- length(doc_list)
names(doc_list) <- paste0('doc', 1:N_docs)

query <- "Healthy cat food"

my_corpus <- VectorSource(c(doc_list, query))
my_corpus$Names <- c(names(doc_list), 'query')
my_corpus <- Corpus(my_corpus)

my_corpus <- my_corpus %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stemDocument) %>% 
  tm_map(removeNumbers) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(stripWhitespace)

term_doc_matrix_stm <- TermDocumentMatrix(my_corpus)
term_doc_matrix <- as.matrix(term_doc_matrix_stm)

termWeight <- function(tf_vec, df) {
  result <- rep(0, length(tf_vec))
  result[tf_vec > 0] <- (1 + log2(tf_vec[tf_vec > 0])) * log2(length(tf_vec) / df)
  return(result)
}

matrixWeightScale <- function(mtx) {
  mtx_weighted_scaled <- apply(mtx, 1, function(x) {
    termWeight(x, sum(x > 0))
  }) %>% 
    t() %>% 
    set_colnames(colnames(mtx)) %>% 
    scale(center = FALSE, scale = sqrt(colSums(.^2)))
  return(mtx_weighted_scaled)
}

querySearch <- function(query, mtx) {
  
}