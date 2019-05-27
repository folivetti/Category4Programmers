def fmap(g, f):
    return lambda x: g(f(x))

def dobra(x):
    return 2*x

def intbool(x):
    return x>3


print(fmap(intbool, dobra)(2))
