import math
'''
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
'''
def primeFindPy(n):
    total = 0
    i = 2
    while total < n:
        if isPrimePy(i):
            total += 1
        i += 1
    print i - 1

def isPrimePy(n):
    res = 0
    for i in range(2, int(math.sqrt(n) + 1)):
        if n % i == 0:
            return False
            res = 1
            break
    if res == 0:
        return True

primeFindPy(10001)