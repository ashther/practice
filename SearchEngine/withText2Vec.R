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
