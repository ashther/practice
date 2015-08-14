# Largest prime factor
# Problem 3
# The prime factors of 13195 are 5, 7, 13 and 29.
# 
# What is the largest prime factor of the number 600851475143 ?

primeFactor <- function(n){
    end <- ceiling(n / 2)
    i <- 2
    for (j in i:end) {
        if (n %% j != 0) {
            next
        } else if (j > i & all((j %% 2:ceiling(j / 2)) != 0)) {
            i <- j
        }
    }
    return(i)
}


for (i in 1e8:2) {
    if (all((i %% 2:ceiling(i / 2)) != 0)) {
        break
    }
}
print(i)