import time
X = [[1, 0.72, 0.32], [1, 0.75, 0.12], [1, 0.53, 0.65], [1, 0.27, 0.82], [1, 0.49, 0.15], [1, 0.02, 0.19],
     [1, 0.35, 0.87], [1, 0.99, 0.71], [1, 0.98, 0.92], [1, 0.73, 0.19],
     [1, 0.72, 0.32], [1, 0.75, 0.12], [1, 0.53, 0.65], [1, 0.27, 0.82], [1, 0.49, 0.15], [1, 0.02, 0.19],
     [1, 0.35, 0.87], [1, 0.99, 0.71], [1, 0.98, 0.92], [1, 0.73, 0.19]]
     
Y = [6.93, 5.99, 1.46, 1.44, 4.51, 1.25, 2.53, 6.88, 6.25, 6.36, 6.93, 5.99, 1.46, 1.44, 4.51, 1.25, 2.53, 6.88, 6.25, 6.36]

a = [0.1, 0.1, 0.1]

def Sum(a, x):
     return sum(w * e for (w,e) in zip(a,x))

def Loss(a, X,Y):
     s = []
     for x in X:
         s.append(sum([w * e for (w,e) in zip(a,x)]))
     return sum([(yy - e)**2 for (yy,e) in zip(Y,s)])

def Descent(a, eta, loss):
     global X, Y
     for x,y in zip(X,Y):
          v = y - Sum(a, x)
          deriv = [ -xx * v for (xx) in x]
          a = [(aa - (dd * eta)) for (aa, dd) in zip(a,deriv)]
          lss = Loss(a, X, Y)
          if lss < loss:
               return [a, lss]
     return None

loss = Loss(a, X, Y)
r = [a, loss]

t = time.process_time()
for iter in range(1000):
     v = Descent(r[0], 0.5, r[1])
     if v == None:
          break
     r = v
elapsed_time = time.process_time() - t
print("Weight Loss Iterations", r[0], r[1], iter)
print(elapsed_time)
