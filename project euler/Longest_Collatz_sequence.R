# evenGo <- function(n){
#     return(n / 2)
# }
# 
# oddGo <- function(n){
#     return(3 * n + 1)
# }
# 
# lcs <- function(n){
#     res <- list(l = 0, N = n)
#     while (n > 1) {
#         if (n %% 2 == 1) {
#             n <- oddGo(n)
#             res$l <- res$l + 1
#         } else {
#             n <- evenGo(n)
#             res$l <- res$l + 1
#         }
#     }
#     res$l <- res$l + 1
#     return(res$l)
# }

lcs <- function(n){
    l <- 0
    while (n > 1) {
        n <- ifelse(n %% 2 == 1, 3 * n + 1, n / 2)
        l <- l + 1
    }
    return(l + 1)
}

func <- function(n){
    l <- 0
    res <- list(length = l, N = NA)
    for (i in 1:n) {
        li <- lcs(i)
        if (li > res$length) {
            res$length <- li
            res$N <- i
        }
    }
    return(res)
}

n <- 1e6
# regular
#  用户  系统  流逝 
# 36.53  0.01 36.58 
system.time(print(func(n)))

# foreach
#  用户  系统  流逝 
# 26.07  1.80 39.25 
require(foreach)
require(doParallel)
system.time({
    cl <- makeCluster(3)
    registerDoParallel(cl)
    test <- foreach(i = 1:n, .combine = 'rbind') %dopar% lcs(i)
    print(c(max(test), which.max(test)))
    stopCluster(cl)
})

# parallel
# 用户  系统  流逝 
# 0.10  0.00 16.09 
require(parallel)
system.time({
    cl <- makeCluster(3)
#     clusterEvalQ(cl, evenGo <- function(n){
#         return(n / 2)
#     })
#     clusterEvalQ(cl, oddGo <- function(n){
#         return(3 * n + 1)
#     })
    res <- parLapply(cl, 1:n, lcs)
    res.df <- do.call('rbind', res)
    print(c(max(res.df), which.max(res.df)))
    stopCluster(cl)
})

















