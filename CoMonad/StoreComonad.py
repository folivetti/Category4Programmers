class Store:
  def __init__(self, f, s):
    self.f = f
    self.s = s

  def fmap(self, g):
    f = lambda s: g(self.f(s))
    return Store(f, self.s)

  def extract(self):
    return self.f(self.s)

  def duplicate(self):
    return Store(lambda s: Store(self.f, s), self.s)

  def extend(self, g):
    return self.duplicate().fmap(g)

  def __str__(self):
    return str([self.f(self.s+i) for i in range(-10,11)])
