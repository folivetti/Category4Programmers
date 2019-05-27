def id(x):
    return x

def compose(f, g):
    return lambda x: f(g(x))

def times2(x):
    return 2*x

print(all( times2(x)==compose(id, times2)(x) for x in range(100) ))
print(all( times2(x)==compose(times2, id)(x) for x in range(100) ))
