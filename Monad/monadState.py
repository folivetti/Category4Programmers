import random

class State:
  # State s -> (a, s)
  def __init__(self, f = None):
    self.r = f

  def run(self, s):
    return self.r(s)

  def unit(self, x):
    return State(lambda s: (x, s))

  def bind(self, k):
    def f(s):
      (a, sn) = self.run(s)
      return k(a).run(sn)
    return State(f)

  
getSt = State(lambda s: (s, s))
setSt = State(lambda s: (None, s))

def mutation(bs):
  if len(bs)==0:
    return State().unit([])
  
  b = bs.pop()

  myRandST = State(myRand)

  def ifthenelse(bsm, p):
    if p < 0.3:
      return State().unit([not b] + bsm)
    else:
      return State().unit([b] + bsm)
  
  return (mutation(bs)
          .bind(lambda bsm: myRandST
                            .bind(lambda p: ifthenelse(bsm, p))
               )
         )

def myRand(s):
  random.setstate(s)
  x = random.random()
  return x, random.getstate()

print(mutation([True, True, False, True]).run(random.getstate())[0])
