from functools import partial
import itertools
import numpy as np
from scipy.ndimage import imread
import matplotlib.pyplot as plt
from StoreComonad import Store

def img2fun(img):
  height, width = img.shape
  def f(coord):
    x, y = coord
    if (0 <= x < height) and (0 <= y < width):
      return img[x,y]
    return -1
  return f

def rebuild(fs, h, w):
  return np.array([fs.f((x,y)) 
                   for x in range(h)
                   for y in range(w)]
                 ).reshape((h,w))

def genMtxRadius(fs, l):
  x,y = fs.s
  half = l//2
  return np.array([fs.f((x+i, y+j))
                   for i in range(-half, half + 1)
                   for j in range(-half, half + 1)]
                  ).reshape((l,l))

def kernel(k, fs):
  if fs.extract() == -1:
    return -1

  mtx = genMtxRadius(fs, k.shape[0])
  idx = mtx != -1 
  v = (mtx[idx]*k[idx]).sum()
  return np.max(v,0)

sharpen = np.array([[0, -1, 0],[-1, 5, -1],[0,-1,0]])
outline = np.array([[-1,-1,-1],[-1,8,-1],[-1,-1,-1]])
emboss  = np.array([[-2,-1,0],[-1,1,1],[0,1,2]])

kSharpen = partial(kernel, sharpen)
kOutline = partial(kernel, outline)
kEmboss  = partial(kernel, emboss)

img = imread("church.jpg", flatten=True)
fs = Store(img2fun(img), (0,0))

fs = fs.extend(kSharpen).extend(kEmboss)
plt.imshow(rebuild(fs, *img.shape), cmap='gray', vmin=0, vmax=255)
plt.show()
