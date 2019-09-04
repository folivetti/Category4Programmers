from functools import partial

class Stream:
  '''
  Data Stream in Python: infinite stream of data with generator function
  -- equivalent to Haskell Stream a = Stream a (Stream a)
  x: initial value
  f: generator function (id if None)
  g: mapped function (Fucntor fmap)
  '''
  def __init__(self, x=1, f=None, g=None):
    idfun = lambda x: x

    self.f = idfun if f is None else f
    self.g = idfun if g is None else g
    self.x = x

  def  __str__(self):
    x = self.extract()
    return f"{x}..."

  def next(self):
    ''' the tail of the Stream '''
    return Stream(self.f(self.x), self.f, self.g)

  def extract(self):
    ''' returns mapped current value '''
    return self.g(self.x)

  def duplicate(self):
    ''' a Stream of Streams '''
    def nextS(xs):
      return self.next()
    return Stream(self, nextS)

  def fmap(self, g):
    ''' Functor instance '''
    return Stream(self.x, self.f, g)

  def extend(self, g):
    ''' comonad extend =>> '''
    return self.duplicate().fmap(g)


def avgN(n, xs):
  s = 0
  for i in range(n):
    s += xs.extract()
    xs = xs.next()
  return s/n

def movAvg(n, xs):
  movAvgN = partial(avgN, n)
  return xs.extend(movAvgN)

def f(x):
  return x+1

xs = Stream(1, f)
print(xs)
print(movAvg(5, xs))
