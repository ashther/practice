evenGo <- function(n){
    return(n / 2)
}

oddGo <- function(n){
    return(3 * n + 1)
}

lcs <- function(n){
    res <- list(l = 0, N = n)
    while (n > 1) {
        if (n %% 2 == 1) {
            n <- oddGo(n)
            res$l <- res$l + 1
        } else {
            n <- evenGo(n)
            res$l <- res$l + 1
        }
    }
    res$l <- res$l + 1
    return(res)
}

# func <- function(n){
#     l <- 0
#     res <- list(length = l, N = NA)
#     for (i in 1:n) {
#         li <- lcs(i)
#         if (li > res$length) {
#             res$length <- li
#             res$N <- i
#         }
#     }
#     return(res)
# }