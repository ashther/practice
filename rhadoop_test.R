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

#====================
train <- data.frame(user = c(rep(1,3), rep(2,4), rep(3,4), rep(4,4), rep(5,6)), 
                     item = c(1,2,3,1,2,3,4,1,4,5,7,1,3,4,6,1,2,3,4,5,6), 
                     perf = c(5, 3, 2.5, 2, 2.5, 5, 2, 2, 4, 4.5, 5, 5, 3, 4.5, 
                             4, 4, 3, 2, 4, 3.5, 4))
require(rhdfs)
require(rmr2)
require(plyr)
hdfs.init()

rmr.options(backend = 'hadoop')

train_hdfs <- to.dfs(keyval(train$user, train))

# 获取所有item的共现组合
train_mr <- mapreduce(input = train_hdfs,
                      map = function(k, v) {
                          keyval(k, v$item) # user->item
                      }, 
                      reduce = function(k, v) {
                          m <- merge(v, v)
                          keyval(m$x, m$y) # all item combination
                      })

# 获取物品与物品之间的共现次数
step2.mr <- mapreduce(input = train_mr, 
                      map = function(k, v) {
                          d <- data.frame(k, v)
                          d2 <- ddply(d, .(k, v), count)
                          keyval(d2$k, d2) # co
                      })

# 以item为键 item-user-perf为值
train2_mr <- mapreduce(input = train_hdfs, 
                       map = function(k, v) {
                           df <- v
                           key <- df$item
                           val <- data.frame(item = df$item, 
                                             user = df$user, 
                                             perf = df$perf) # item, user, perf
                           keyval(key, val)
                       })

# 联合物品共现矩阵与物品被评分矩阵
eq_hdfs <- equijoin(left.input = step2.mr, 
                    right.input = train2_mr, 
                    map.left = function(k, v) {
                        keyval(k, v)
                    }, 
                    map.right = function(k, v) {
                        keyval(k, v)
                    }, 
                    outer = c('left')) # item1,item2,freq,item1,user,perf

# group by 同一用户、物品k、物品v，预测评分=对k的评分乘以k和v的共现次数
cal_mr <- mapreduce(input = eq_hdfs, 
                    map = function(k, v) {
                        val <- v
                        na <- is.na(v$user.r)
                        if (length(which(na)) > 0) {
                            val <- v[-which(is.na(v$user.r)), ]
                        }
                        keyval(val$k.l, val)
                    }, 
                    reduce = function(k, v) {
                        val <- ddply(v, .(k.l, v.l, user.r), summarize, v = freq.l * perf.r)
                        keyval(val$k.l, val) # item1, item2, user, perf
                    })

# 取出用户、物品v和预测评分，排序
result_mr <- mapreduce(input = cal_mr, 
                       map = function(k, v) {
                           keyval(v$user.r, v)
                       }, 
                       reduce = function(k, v) {
                           val <- ddply(v, .(user.r, v.l), summarize, v = sum(v))
                           val2 <- val[order(val$v, decreasing = TRUE), ]
                           names(val2) <- c('user', 'item', 'perf')
                           keyval(val2$user, val2)
                       })

