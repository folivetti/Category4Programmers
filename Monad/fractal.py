import matplotlib.pyplot as plt

class ListMonad:
  # return/unit
  def __init__(self, x):
    self.xs = [x]

  def join(self):
    self.xs = [xij for xi in self.xs for xij in xi]
    return self

  def fmap(self, f):
    self.xs = [f(x) for x in self.xs]
    return self

  def bind(self, f):
    self.fmap(f).join()
    return self

def genCantor(x):
  return [(x[0], (2*x[0] + x[1])/3), ((x[0] + 2*x[1])/3, x[1])]

seed = (0.0, 1.0)
cantor = ListMonad(seed)
n = 10

for it in range(n):
  for p in cantor.xs:
    plt.plot(list(p), [it,it], color="k", lw=n, solid_capstyle="butt")
  cantor.bind(genCantor)

plt.show()
