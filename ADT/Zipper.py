class Tree:
  def __init__(self, x=None, left=None, right=None):
    self.x = x
    self.left = left
    self.right = right    

  def __str__(self):
    return f"({self.left} {self.x} {self.right})"

class Zipper:
  def __init__(self, t):
    self.left = t.left
    self.right = t.right
    self.focus = [(False, t.x, None)]

  def __str__(self):
    return f"#{self.left} # {self.right} # {self.focus}#"

  def esq(self):
    left, right = self.left, self.right
    if left is not None:
      self.left = left.left
      self.right = left.right
      self.focus.append((False, left.x, right))
    return self
    
  def dir(self):
    left, right = self.left, self.right
    if right is not None:
      self.left = right.left
      self.right = right.right
      self.focus.append((True, right.x, left))
    return self

  def upward(self):
    if len(self.focus) > 1:
      (dir, x, t) = self.focus.pop(-1)
      if dir: 
        self.right = Tree(x, self.left, self.right)
        self.left = t
      else:
        self.left = Tree(x, self.left, self.right)
        self.right = t
    return self

n3 = Tree(3)
n4 = Tree(4)
n5 = Tree(5)
n2 = Tree(2, n4, n5)
n1 = Tree(1, n2, n3)

z = Zipper(n1)
print(z, "\n")

z.esq()
print(z, "\n")

z.esq()
print(z, "\n")

z.dir()
print(z, "\n")

z.upward().upward()
print(z, "\n")
