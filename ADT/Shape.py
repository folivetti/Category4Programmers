from abc import ABC, abstractmethod
from math import pi

class Shape(ABC):
    @abstractmethod
    def area(self):
        pass

    @abstractmethod
    def circ(self):
        pass

class Circle(Shape):
    def __init__(self, r):
        self.r = r

    def area(self):
        return pi*self.r*self.r
    def circ(self):
        return 2.0*pi*self.r

class Rect(Shape):
    def __init__(self, w, h):
        self.w = w
        self.h = h
    def area(self):
        return self.w * self.h
    def circ(self):
        return 2.0*(self.w + self.h)

class Square(Shape):
    def __init__(self, s):
        self.s = s
    def area(self):
        return self.s*self.s
    def circ(self):
        return 4.0*self.s

circ = Circle(2.0)
print(circ.area())
