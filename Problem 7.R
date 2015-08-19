# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
# we can see that the 6th prime is 13.
# What is the 10001st prime number?

primeFind <- function(n){
    temp <- 2:n
    for (i in 2:(ceiling(n / 2))) {
        temp <- temp[!temp %in% i * ]
    }
}

isPrime <- function(n){
    if (all(n %% (2:ceiling(sqrt(n))) != 0)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

system.time(
for (i in 1e9:1) {
    if (isPrime(i)) {
        print('yes')
        break
    }
})
