#By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
#we can see that the 6th prime is 13.#

#What is the 10 001st prime number?

function primeFind(n)
    #function body
    total = 0
    i = 2
    while total < n
        if isprime(i)
            total += 1
        end
        i += 1
    end
    print(i - 1)
end