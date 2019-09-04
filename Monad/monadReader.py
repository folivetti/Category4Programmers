from collections import namedtuple

'''
Config struct and functions
'''
Conf = namedtuple('Conf', ['numberOfElems', 'filtro', 'transform'])

def numberOfElems(c):
  return c.numberOfElems
def filtro(c):
  return c.filtro
def transform(c):
  return c.transform

'''
Reader Monad class
'''
class Reader():
  # Reader e a
  def __init__(self, fun = None):
    self.r = fun

  def run(self, e):
    return self.r(e)

  # (a -> b) -> Reader e a -> Reader e b
  def fmap(self, f):
    return Reader(lambda e: f(self.r(e)))

  def unit(self, x):
    return Reader(lambda e: x)

  # Reader e a -> (a -> Reader e b) -> Reader e b
  def bind(self, fab):
    def f(e):
      a = self.r(e)
      rb = fab(a)
      return rb.run(e)
    return Reader(f)
    
def composeMonad(f, g):
  def k(a):
    return f(a).bind(g)
  return k

def ask(e):
  return e

def askFor(f):
  return Reader(ask).fmap(f)

'''
Example of use
'''
def recupera(tipo, xs):
  if tipo == "filtra":
    return filtra(xs)
  else:
    return transforma(xs)

def filtra(xs):
  f = composeMonad( composeMonad(aplicaFiltro, pega),
                    aplicaMap
                  )
  return f(xs)
  
def transforma(xs):
  f = composeMonad(pega, aplicaMap)
  return f(xs)
    
def aplicaFiltro(xs):
  filtroXS = lambda f: filter(f, xs)
  return askFor(filtro).fmap(filtroXS)

def aplicaMap(xs):
  mapXS = lambda f: map(f, xs)
  return askFor(transform).fmap(mapXS)

def pega(xs):
  pegaXS = lambda n: list(xs)[:n]
  return askFor(numberOfElems).fmap(pegaXS)
  
def bindTo(p, f):
    return askFor(p).bind(lambda x: Reader().unit(f(x)))

def even(x):
  return x%2==0
def square(x):
  return x*x
    
myCF = Conf(10, even, square)
print( list(recupera("transforma", range(1, 100)).run(myCF)) )
print( list(recupera("filtra", range(1, 100)).run(myCF)) )
