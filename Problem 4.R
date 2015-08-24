# A palindromic number reads the same both ways. 
# The largest palindrome made from the product of two 2-digit numbers is 
# 9009 = 91 × 99.
# Find the largest palindrome made from the product of two 3-digit numbers.

isPali <- function(n){
    temp <- unlist(strsplit(as.character(n), ''))
    pmet <- temp[length(temp):1]
    if (all(temp == pmet)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

temp <- 0
for (i in 999:1) {
    for (j in 999:1) {
        if (isPali(i * j) & (i * j) > temp) {
            temp <- i * j
        }
    }
}

