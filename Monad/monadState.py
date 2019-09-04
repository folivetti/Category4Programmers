import random
from functools import partial

class State:
  # State s -> (a, s)
  def __init__(self, f = None):
    self.r = f

  def run(self, s):
    return self.r(s)

  def unit(self, x):
    return State(lambda s: (x, s))
    
  def fmap(self, f):
    return State(lambda s: applytoState(f,self.r(s)))

  def bind(self, k):
    def f(s):
      (a, sn) = self.run(s)
      return k(a).run(sn)
    return State(f)
    
def applytoState(f, st):
  return (f(st[0]), st[1])
  
def change(b, p):  
  if p < 0.3:
    return not b #State().unit(not b)
  return b #State().unit(b)

# [State bool] -> State [bool]
def sequence(ss):
  def f(s):
    xs = []
    for si in ss:
      (x, s) = si.run(s)
      xs.append(x)
    return (xs, s)
  return State(f)

def mutation(bs):
  return sequence([myRandST.fmap(partial(change,b)) 
                         for b in bs])

def select(b, p):
  if p < 0.3 and b:
    return [b]
  return []
  
def concat(xss):
  return [x for xs in xss
              for x in xs]
              
def selection(bs):  
  return sequence([myRandST.fmap(partial(select,b)) 
                         for b in bs]).fmap(concat)
                         
def myRand(s):
  random.setstate(s)
  x = random.random()
  return x, random.getstate()

myRandST = State(myRand)

bs = [True]*10
bs1 = (mutation(bs)
        .bind(selection)
        .run(random.getstate())
      )

print( bs, bs1[0] )
