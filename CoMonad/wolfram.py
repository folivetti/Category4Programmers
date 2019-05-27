from functools import partial
import itertools
from StoreComonad import Store

def rule(x, fs):
  succDiv2 = [x]
  while x!=0:
    x = x//2
    succDiv2.append(x)
  bit = fs.f(fs.s+1) + 2*fs.f(fs.s) + 4*fs.f(fs.s-1)
  if bit >= len(succDiv2):
    return 0
  return succDiv2[bit] % 2
  
def wolfram(rl, fs):
  while True:
    yield fs
    fs = fs.extend(rl)

def f0(x):
  if x==0:
    return 1
  return 0

fs = Store(f0, 0)

wolf30 = wolfram(partial(rule, 30), fs)
top5 = itertools.islice(wolf30, 6)

for w in top5:
  print(w)

