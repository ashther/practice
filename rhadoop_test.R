library(rhdfs)
library(rmr2)

hdfs.init()
#===================
small.ints <- to.dfs(1:10)
mapreduce(input = small.ints, map = function(k, v)cbind(k, v))
from.dfs('/tmp/file19f73df21734') #ajust
#===================
webpages <- read.csv('webpages.csv', stringsAsFactors = FALSE)

webpage.dfs <- to.dfs(webpages)
total_visit <- sum(webpages$visits)
key <- NA
val <- NULL

mapper_1 <- function(k, v) {
  key <- v[2]
  val <- v[3]
  keyval(key, val)
}

reducer_1 <- function(k, v) {
  per <- sum(v) / total_visit * 100
  if (per > 67) {
    val <- 'high'
  } else if(per > 33 & per <= 67) {
    val <- 'median'
  } else {
    val <- 'low'
  }
  keyval(k, val)
}

mapper_2 <- function(k, v) {
  keyval(v, k)
}

reducer_2 <- function(k, v) {
  if (is.na(key)) {
    key <- k
    val <- v
  } else {
    if (key == k) {
      val <- c(val, v)
    } else {
      key <- k
      val <- v
    }
  }
  keyval(key, list(val))
}

mapreduce(webpage.dfs, map = mapper_1, reduce = reducer_1) %>%
  mapreduce(map = mapper_2, reduce = reducer_2, combine = TRUE) %>%
  from.dfs()

#====================
wc.map <- function(k, v) {
  keyval(unlist(strsplit(v, ' ')), 1)
}

wc.reduce <- function(word, counts) {
  keyval(word, sum(counts))
}

test_input <- to.dfs('this is a test, test rhadoop wordcount script, rhadoop is greate')
mapreduce(input = test_input, map = wc.map, reduce = wc.reduce, combine = TRUE)


