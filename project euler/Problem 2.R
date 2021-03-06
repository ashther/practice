# Even Fibonacci numbers
# Problem 2
# Each new term in the Fibonacci sequence is generated by adding the previous two terms. 
# By starting with 1 and 2, the first 10 terms will be:
#     
#     1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
# 
# By considering the terms in the Fibonacci sequence whose values do not exceed 
# four million, find the sum of the even-valued terms.

fib_even <- function(n){
    res <- c(1, 2)
    while (max(res) <= n) {
        res <- append(res, res[length(res)] + res[length(res) - 1])
    }
    res <- res[1:(length(res) - 1)]
    res <- res[res %% 2 == 0]
    return(sum(res))
}