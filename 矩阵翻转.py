#http://nanti.jisuanke.com/t/5
'''
输入：
4 4 1
1 2 3 4
5 6 7 8
9 0 1 2
3 4 5 6
输出：
3 4 5 6 
9 0 1 2 
5 6 7 8 
1 2 3 4 
'''

#coding = utf-8
def transpose(inp, m, n):
    output = [[0 for _ in range(m)] for _ in range(n)]
    for i in range(m):
        for j in range(n):
            output[j][i] = inp[i][j]
    return output

def printf(inp):
    for row in inp:
        for num in row:
            print num,
        print ''
        
m, n, t = [int(x) for x in raw_input().split(' ')]
inp = list()
for i in range(m):
    inp.append([int(x) for x in raw_input().split(' ')])

if t == 1:
    printf(inp[::-1])
else:
    tmp = transpose(inp, m, n)
    tmp = tmp[::-1]
    tmp = transpose(tmp, n, m)
    printf(tmp)
