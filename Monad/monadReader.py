from collections import namedtuple

'''
Config struct and functions
'''
Conf = namedtuple('Conf', ['alg', 'thr', 'it'])

def alg(c):
  return c.alg
def thr(c):
  return c.thr
def it(c):
  return c.it

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

def ask(e):
  return e

def askFor(f):
  return Reader(ask).fmap(f)

'''
Example of use
'''

def go(it, xs):
  ys = []
  for x in xs:
    ys.append(it*x)
    it = it - 1
  return ys

def f3(xs):
  # ask for it and pass to 'runGo'
  runGo = lambda t: Reader().unit(go(t, xs))
  return (askFor(it)
          .bind(runGo))

def f2(xs):
  # ask for a thr and pass to 'gof3'
  gof3 = lambda t: f3(filter(lambda x: x<t, xs))
  return (askFor(thr)
          .bind(gof3))

def algorithm(xs):
  f = {"f2" : f2, "f3" : f3}
  choice = lambda algo: f[algo](xs)
  return (askFor(alg)
          .bind(choice))

c = Conf("f2", 2.5, 5)
print(algorithm(range(1,11))
      .run(c))
