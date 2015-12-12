library(rhdfs)
library(rmr2)

hdfs.init()

small.ints <- to.dfs(1:10)
mapreduce(input = small.ints, map = function(k, v)cbind(v, v^2))
from.dfs('/tmp/file92e6f53a2d8') #ajust
