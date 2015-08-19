# The sum of the squares of the first ten natural numbers is, 
# 12 + 22 + ... + 102 = 385 
# 
# The square of the sum of the first ten natural numbers is, 
# (1 + 2 + ... + 10)2 = 552 = 3025 
# 
# Hence the difference between the sum of the squares 
# of the first ten natural numbers and the square 
# of the sum is 3025 − 385 = 2640. 
# 
# Find the difference between the sum of the squares of the first one 
# hundred natural numbers and the square of the sum.

func1 <- function(n){
    return(sum(1:n) ^ 2 - sum((1:n) ^ 2))
}

func2 <- function(n){
    temp <- combn(1:n, 2)
    return(sum(temp[1, ] * temp[2, ]) * 2)
}

# microbenchmark(func1, func2)
# Unit: nanoseconds
# expr min lq mean median uq max neval
# func1   0  0 3.47      0  1 321   100
# func2   0  0 0.21      0  0   1   100

#25164150

