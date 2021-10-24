d = [{"a":2, "b":3}, {"c":2, "d":3}, {"e":2, "f":3}]
i = 2
vct = [10, 20, 30, 40]
vct[13] = 40
vct[1:i+1] = 40
i = 20
def test(k, v):
    for u in range(0,k + 2,1):
        if u+3==5:
            u += 2*v
            print("U==2:", u)
        else:
            u -= 1-v
            print("U:", u)
    while (v > 10):
        print(v)
        v -= 2
    println(vct)
    k = 0
    for i in d[k+1]:
        print(i)
    
test(i, 100)
