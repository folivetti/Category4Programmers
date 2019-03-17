def bind(xs, k):
    return (y for x in xs for y in k(x))
    
def geralista(x):
    return [2*x]
    
def triples(n):
    return ( (x,y,z) for z in range(1, n+1) 
                     for x in range(1, z+1)
                     for y in range(x, z+1)
                     if (x**2 + y**2 == z**2))
                   
for t in triples(10):
  print(t)
  
print(list(bind([1,2,3], geralista)))
