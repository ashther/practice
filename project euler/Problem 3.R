# Largest prime factor
# Problem 3
# The prime factors of 13195 are 5, 7, 13 and 29.
# 
# What is the largest prime factor of the number 600851475143 ?

prime.series <- function(n){
    res <- rep(0, length(n))
    for (i in 1:length(n)) {
        if (ceiling(n[i]) - n[i] > 0) {
            next
        } else if (all(n[i] %% 2:ceiling(sqrt(n[i])) != 0) | n[i] == 2) {
            res[i] <- 1
        }
    }
    return(res)
}

pffind <- function(n){
    #border <- sqrt(n)
    border <- 1e8
    test <- (n %% (2:border) == 0) & (prime.series(n / (2:border)))
    
    for (i in 1:border) {
        if (test[i] == TRUE) {
            print(n / i)
            break
        }
    }
}

