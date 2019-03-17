from functools import singledispatch

class Maybe:
    def __init__(self, x = None):
        self.val = x

@singledispatch
def fmap(a, f):
    print("Not implemented for" + str(type(a)))

@fmap.register(list)
def _(l, f):
    return list(map(f, l))
    
@fmap.register(Maybe)
def _(m, f):
    if m.val is None:
        m.val = None
    else:
        m.val = f(m.val)
    return m
    
f = lambda x: x*2

l = [1,2,3]
m1 = Maybe(2)
m2 = Maybe()

print(fmap(l, f))
print(fmap(m1, f).val)
print(fmap(m2, f).val)
