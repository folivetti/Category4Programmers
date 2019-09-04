from functools import partial
import itertools
import numpy as np
from scipy.ndimage import imread
import matplotlib.pyplot as plt
from StoreComonad import Store

def img2fun(img):
  '''
  img2fun : img
  retorna uma função f que recebe uma tupla
  (x, y) representando a coordenada e retorna o pixel
  da imagem img
  '''
  height, width = img.shape
  
  def f(coord):
    # Complete: A função deve verificar se a coordenada está dentro da faixa
    # 0 <= x < height e 0 <= y < width
    # Se sim, retorna a coordenada x,y de img
    # Caso contrário retorne -1
  return f

def rebuild(fs, h, w):
  '''
  rebuild : Store (Int, Int) Int -> Int -> Int -> [[Int]]
  retorna uma imagem de dimensão h x w reconstruída pleo
  comonad Store fs
  '''
  # Complete: fs encapsula uma função que recebe um estado (x,y) e 
  # retorna esse pixel da imagem. Faça uma função para reconstruir uma
  # array contendo os pixels de uma imagem de dimensão h x w.
  # Dica: você pode gerar uma lista com os pixels na sequencia e
  # utilizar reshape((h,w)) do numpy para reorganizá-la como uma array.
  return imgMtx

def genMtxRadius(fs, l):
  '''
  genMtxRadius : Store (Int, Int) Int -> Int -> [[Int]]
  retorna a submatrix da imagem centrada no estado atual
  e de  raio l/2
  '''
  # Complete: similar a função anterior, agora você deve retornar
  # uma matriz de dimensão l x l contendo as linhas 
  # [x - l/2, x + l/2] e colunas [y - l/2, y + l/2]
  # centradas nas coordenadas do estado atual
  return imgMtx
  
def kernel(k, fs):
  '''
  Aplica um kernel k no estado atual da imagem definida por fs.
  '''
  
  # Se o estado atual é invalido, retorna -1
  if fs.extract() == -1:
    return -1

  # Pega a submatriz do tamanho do kernel
  mtx = genMtxRadius(fs, k.shape[0])
  
  # aplica o kernel em todos os pixels válidos
  idx = mtx != -1 
  v = (mtx[idx]*k[idx]).sum()
  
  return np.max(v,0)

# Kernels exemplo
sharpen = np.array([[0, -1, 0],[-1, 5, -1],[0,-1,0]])
outline = np.array([[-1,-1,-1],[-1,8,-1],[-1,-1,-1]])
emboss  = np.array([[-2,-1,0],[-1,1,1],[0,1,2]])

# Aplicação parcial para gerar funções Store (Int, Int) Int -> Int
kSharpen = partial(kernel, sharpen)
kOutline = partial(kernel, outline)
kEmboss  = partial(kernel, emboss)

# Leia a imagem e cria um Store centrado em (0,0)
img = imread("church.jpg", flatten=True)
fs = Store(img2fun(img), (0,0))

# Aplica os Kernels
fs = (fs
      .extend(kSharpen)
      .extend(kEmboss)
     )
     
# Mostra a imagem
plt.imshow(rebuild(fs, *img.shape), cmap='gray', vmin=0, vmax=255)
plt.show()
