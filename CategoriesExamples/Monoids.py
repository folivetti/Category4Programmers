from functools import singledispatch
from collections import namedtuple

@singledispatch
def mempty(a):
    raise Error("Not implemented for" + a)

@singledispatch
def mappend(a, b):
    raise Error("Not implemented for" + a)

@mempty.register(int)
def _(a):
    return 1

@mappend.register(int)
def _(a,b):
    return a * b

Produto = namedtuple('Produto', ['qtd', 'preco'])

@mempty.register(Produto)
def _(a):
    return Produto(0, 0.0)

@mappend.register(Produto)
def _(a,b):
    return Produto(a[0] + b[0], a[1] + b[1])
    
def soma_produtos(ps):
    return reduce(mappend, ps)
    
def zera_estoque(p):
    return mempty(p)
