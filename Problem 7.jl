
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