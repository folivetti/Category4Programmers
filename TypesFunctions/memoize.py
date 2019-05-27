import time
import random

def memoize(f):
    mem = {}
    def memf(x):
        if x not in mem:
            mem[x] = f(x)
        return mem[x]
    return memf

def fibo(n):
    if n == 0: return 0
    if n == 1: return 1
    return fibo(n-1) + fibo(n-2)

def rand_seed(seed):
    random.seed(seed)
    return random.random()

f = memoize(fibo)
t1 = time.time()
print(f(30))
t2 = time.time()
print(f(30))
t3 = time.time()

diff1 = t2-t1
diff2 = t3-t2
print(f"First call: {diff1}, Second call: {diff2}")

g = memoize(random.randrange)
m1, m2, m3 = g(100), g(100), g(200)
n1, n2, n3 = random.randrange(100), random.randrange(100), random.randrange(200)
print(f"Random numbers: {m1}, {m2}, {m3}")
print(f"Memoized random numbers: {n1}, {n2}, {n3}")

g = memoize(rand_seed)
m1, m2, m3 = g(100), g(100), g(200)
n1, n2, n3 = rand_seed(100), rand_seed(100), rand_seed(200)
print(f"Random numbers: {m1}, {m2}, {m3}")
print(f"Memoized random numbers: {n1}, {n2}, {n3}")
