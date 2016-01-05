library(rhdfs)
library(rmr2)

hdfs.init()
#===================
small.ints <- to.dfs(1:10)
mapreduce(input = small.ints, map = function(k, v)cbind(k, v))
from.dfs('/tmp/file19f73df21734') #ajust
#===================
webpages <- read.csv('webpages_mapreduce.csv', stringsAsFactors = FALSE)
totalvisits <- sum(webpages$visits)

webpages.hdfs <- to.dfs(webpages)
wp.mapper1 <- function(k, v){
  key <- v[1]
  val <- v[3]
  keyval(key, val)
}

wp.reducer1 <- function(k, v){
  per <- sum(v) / totalvisits * 100
  keyval(k, per)
}

mapreduce(input = webpages.hdfs, map = wp.mapper1, reduce = wp.reducer1)
result <- from.dfs('/tmp/file19f71ab2f8ee')

#====================
wc.map <- function(k, v) {
  keyval(unlist(strsplit(v, ' ')), 1)
}

wc.reduce <- function(word, counts) {
  keyval(word, sum(counts))
}

test_input <- to.dfs('this is a test, test rhadoop wordcount script, rhadoop is greate')
mapreduce(input = test_input, map = wc.map, reduce = wc.reduce, combine = TRUE)


