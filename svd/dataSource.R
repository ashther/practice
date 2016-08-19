library(dplyr)
library(magrittr)

pu <- list(list(c(0,0,1),c(0,1,22),c(0,2,1),c(0,3,1),c(0,5,0)),
           list(c(1,0,1),c(1,1,32),c(1,2,0),c(1,3,0),c(1,4,1),c(1,5,0)),
           list(c(2,0,0),c(2,1,18),c(2,2,1),c(2,3,1),c(2,4,0),c(2,5,1)),
           list(c(3,0,1),c(3,1,40),c(3,2,1),c(3,3,0),c(3,4,0),c(3,5,1)),
           list(c(4,0,0),c(4,1,40),c(4,2,0),c(4,4,1),c(4,5,0)),
           list(c(5,0,0),c(5,1,25),c(5,2,1),c(5,3,1),c(5,4,1)))

pv <- list(list(c(0,0,1),c(0,1,1),c(0,2,0),c(0,3,1),c(0,4,0),c(0,5,0)),
       list(c(1,0,22),c(1,1,32),c(1,2,18),c(1,3,40),c(1,4,40),c(1,5,25)),
       list(c(2,0,1),c(2,1,0),c(2,2,1),c(2,3,1),c(2,4,0),c(2,5,1)),
       list(c(3,0,1),c(3,1,0),c(3,2,1),c(3,3,0),c(3,5,1)),
       list(c(4,1,1),c(4,2,0),c(4,3,0),c(4,4,1),c(4,5,1)),
       list(c(5,0,0),c(5,1,0),c(5,2,1),c(5,3,1),c(5,4,0)))

V <- c(0.15968384, 0.9441198 , 0.83651085, 0.73573009, 0.24906915, 0.85338239,
       0.25605814, 0.6990532 , 0.50900407, 0.2405843 , 0.31848888, 0.60233653,
       0.24237479, 0.15293281, 0.22240255, 0.03943766, 0.19287528, 0.95094265) %>% 
  matrix(ncol = 3, byrow = TRUE)

U <- matrix(0, nrow = 6, ncol = 3)
L <- 0.03

uvCaculate <- function(p, V) {
  sapply(p, function(x) {
    vo <- V[do.call(rbind, x) %>% `[`(, 2) %>% `+`(1), ]
    pvo <- do.call(rbind, x) %>% `[`(, 3)
    ur <- t(solve(t(vo) %*% vo + L * diag(3))) %*% t(vo) %*% pvo
  }) %>% 
    t()
}

for (i in 1:5) {
  U <- uvCaculate(pu, V)
  V <- uvCaculate(pv, U)
  
  err <- sapply(pu, function(x) {
    temp <- do.call(rbind, x)
    sum(((temp[, 3] - U[temp[, 1] + 1, ] %*% t(V[temp[, 2] + 1, ])) ^ 2) * 
          diag(nrow = nrow(temp), ncol = nrow(temp)))
  }) %>% 
    sum()
  n <- sapply(pu, length) %>% sum()
  print(sqrt(err / n))
}
