from math import sqrt

def safe_root(x):
    if x>=0:
        return sqrt(x)
    else:
        return None

def safe_reciprocal(x):
    if x!=0:
        return 1.0/x
    else:
        return None

def fish(f, g):
    def h(x):
        z = f(x)
        if z is None:
            return z
        else:
            return g(z)
    return h

print(safe_root(16))
print(safe_root(-16))

process = fish(safe_root, safe_reciprocal)
print(process(2))
print(process(0))
print(process(-1))
