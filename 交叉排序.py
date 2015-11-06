# http://www.jisuanke.com/course/95/2813

test = [int(x) for x in raw_input().split()]
result = test

even_list = list()
even_ids = list()
odd_list = list()
odd_ids = list()

for ids, num in enumerate(test):
    if (ids+1) % 3 != 0 and (ids+1) % 2 == 0:
        even_list.append(num)
        even_ids.append(ids)
    elif (ids+1) % 3 == 0:
        odd_list.append(num)
        odd_ids.append(ids)
even_list.sort()
odd_list.sort(reverse = True)

for ids, num in enumerate(test):
    if ids in even_ids:
        result[ids] = even_list.pop(0)
    elif ids in odd_ids:
        result[ids] = odd_list.pop(0)
for num in result:
    print num,
