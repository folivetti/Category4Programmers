def bimap(f, g, t):
    return (f(t[0]), g(t[1]))
    
print(bimap( str, int, (23, "23") ))
print(bimap( sum, lambda x: x/2, ([1,2,3], 4) ))
