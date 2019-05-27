from collections import namedtuple

Writer = namedtuple('Writer', ['val', 'log'])

def id_writer(x):
    return Writer(x, "")
    
def compose_writer(m1, m2):
    def f(a):
        b, s1 = m1(a)
        c, s2 = m2(b)
        return Writer(c, s1+s2)
    return f
    
def notW(b):
    return (not b, "not")

def is_even(x):
    return (x%2==0, "even")
    
def is_odd(x):
    return compose_writer(is_even, notW)(x)
    
    
print(is_odd(7))
