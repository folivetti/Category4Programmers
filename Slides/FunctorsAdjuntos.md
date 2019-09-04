---
title: "Teoria das Categorias para Programadores"
author: Fabrício Olivetti de França
date: 17 de Agosto de 2019
mainfont: DejaVu Sans
fontsize: 12pt
---

# Functors Adjuntos

## Isomorfismos de Categorias {.fragile}

Como podemos definir o isomorfismo entre duas categorias $C, D$? 

## Isomorfismos de Categorias {.fragile}

Os morfismos entre duas categorias são Functors, duas categorias são isomorfas se temos dois Functors, $R : C \rightarrow D$ e $L : D \rightarrow C$, tal que a composição deles forma o Functor identidade.

## Isomorfismos de Categorias {.fragile}

$$R \circ L = I_D$$

e

$$L \circ R = I_C$$

## Isomorfismos de Categorias {.fragile}

Lembrando que o Functor identidade é dado por:

```Haskell
data Identity a = Identity a

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)
```

## Isomorfismos de Categorias {.fragile}

Para provar que duas categorias são isomórficas, precisamos definir transformações naturais entre a composição dos Functors e o Functor Identidade:

```Haskell
eta  :: Id    -> (R.L)
eta' :: (R.L) -> Id

eps  :: (L.R) -> Ic
eps' :: Ic    -> (L.R)
```

## Functors Adjuntos {.fragile}

Dizemos que dois Functors `R, L`{.haskell} são adjuntos se eles possuem as transformações naturais `eta, eps`{.haskell}, mas sem a necessidade de existir `eta', eps'`{.haskell}.

## Functors Adjuntos {.fragile}

- O Functor `L` é denominado adjunto esquerdo (*left adjunct*) 
- O Functor `R` é o adjunto direito (*right adjunct*)
- `eta` é chamado de `unit`
- `eps` de `counit`. 

Denotamos $L \dashv R$ como $L$ é o adjunto esquerdo de $R$. 

## Functors Adjuntos {.fragile}
   
A função `eta` pega um objeto de `D` e faz um *passeio* entre as categorias `C, D` utilizando os Functors `R . L`, retornando um outro objeto de `D` encapsulado no functor `R . L`.

\centering
\begin{tikzpicture}[auto, scale=1, transform shape]
\tikzset{my node/.style={node distance=0.5cm}}
\usetikzlibrary{calc}

\node (d) {$d$};
\node (dp) [below of=d] {$d'$};
\node (c) [below left of=d, xshift=-25mm] {$c$};

\draw[thick] ($(c.north)+(0.0,-0.3)$)  circle (1);
\draw[thick] ($(d.north)+(-0.1,-0.6)$)  circle (1);

\node[above] at ($(c.north)+(-0.1,1.2)$) {$\mathbf{C}$};
\node[above] at ($(d.north)+(-0.1,0.8)$) {$\mathbf{D}$};

\draw[->] (c) edge (dp);
\draw[->] (d) edge node[above] {$L$} (c);
\draw[->] (d) edge node[right] {$\eta$} (dp);
\end{tikzpicture}

```Haskell
eta  :: Identity d -> (R.L) d
```

## Functors Adjuntos {.fragile}

A função `eps` indica como extrair um `c` do Functor `L . R` passando pela categoria `D`.

\centering
\begin{tikzpicture}[auto, scale=1, transform shape]
\tikzset{my node/.style={node distance=0.5cm}}
\usetikzlibrary{calc}

\node (cp) {$c'$};
\node (c) [below of=cp] {$c$};
\node (d) [below right of=cp, xshift=25mm] {$d$};

\draw[thick] ($(d.north)+(0.0,-0.3)$)  circle (1);
\draw[thick] ($(cp.north)+(-0.1,-0.6)$)  circle (1);

\node[above] at ($(d.north)+(-0.1,1.2)$) {$\mathbf{D}$};
\node[above] at ($(cp.north)+(-0.1,0.8)$) {$\mathbf{C}$};

\draw[->] (d) edge (cp);
\draw[->] (c) edge node[below] {$R$} (d);
\draw[->] (cp) edge node[left] {$\epsilon$} (c);
\end{tikzpicture}

```Haskell
eps  :: (L.R) c -> Identity c
```

## Functors Adjuntos {.fragile}

Em outras palavras, o `unit` (também chamado de `return` e `pure` em outros contextos) permite introduzir um container ou Functor `R.L` em todo tipo `d`. 

```Haskell
-- F = R . L
unit :: d -> F d

```

## Functors Adjuntos {.fragile}

Por outro lado, o `counit` (em algumas linguagens conhecido como `extract`) permite retirar um objeto de um container ou Functor.

```Haskell
-- G = L . R
counit :: G c -> c
```

## Functors Adjuntos {.fragile}

A classe de Functors adjuntos é definido como:

```Haskell
class (Functor f, Representable y) =>
  Adjunction f u | f -> u, u -> f where
    unit   :: x -> u (f x)
    counit :: f (u x) -> x
```

## Functors Adjuntos {.fragile}

A condição `f -> u, u -> f` é uma **dependência funcional** e implica que só pode existir uma única instância para `f` na esquerda e para `u` a direita.

## Functors Adjuntos {.fragile}

Ou seja, se eu defino `Adjunction [] (Reader a)`, não posso definir `Adjunction Maybe (Reader a)` nem `Adjunction [] Maybe`. 

## Functors Adjuntos {.fragile}

Junto dessas duas funções também podemos definir essa classe através de `leftAdjunction` e `rightAdjunction`:

```Haskell
class (Functor f, Representable y) =>
  Adjunction f u | f -> u, u -> f where
    leftAdjunct  :: (f x -> y) -> x -> u y
    rightAdjunct :: (x -> u y) -> f x -> y
```

## Functors Adjuntos {.fragile}

E elas se relacionam da seguinte forma:

```Haskell
unit         = leftAdjunct id
counit       = rightAdjunct id
leftAdjunct  = fmap g . unit
rightAdjunct = counit . fmap g
```

## Functors Adjuntos {.fragile}

Existem poucos Functors pertencentes a $\mathbf{Hask}$ que são adjuntos, porém a combinação $L.R$ e $R.L$ formam os conceitos de Monads e Comonads, conforme veremos mais adiante.

## Curry e Uncurry {.fragile}

Um exemplo de Functors adjuntos no Haskell são `(,a)` e `Reader a = (a ->)`. Podemos definir a instância como:

```Haskell
instance Adjunction (,a) (Reader a) where
  -- unit :: c -> Reader a (c,a)
  unit c = \a -> (c, a)
  
  -- counit :: (Reader a c, a) -> c
  counit (f, c) = f c
```

## Curry e Uncurry {.fragile}

Podemos definir `leftAdjunct` como:

```Haskell
-- f = (,a); u = (a ->)
-- leftAdjunct :: ((x,a) -> y) -> x -> (a -> y)
leftAdjunct g x = (fmap g . unit) x
= (fmap g) (unit x)
= (fmap g) (\a -> (x, a))
= g . (\a -> (x, a))
= \a -> g (x,a)
```

## Curry e Uncurry {.fragile}

ou...

```Haskell
-- f = (,a); u = (a ->)
-- leftAdjunct :: ((x,a) -> y) -> (x -> a -> y)
leftAdjunct g = \x -> \a -> g (x,a)
```

## Curry e Uncurry {.fragile}

E `rightAdjunct` como:

```Haskell
-- f = (,a); u = (a ->)
-- rightAdjunct :: (x -> (a -> y)) -> (x,a) -> y
rightAdjunct g (x,a) = counit . (fmap g) (x,a)
= counit (g x, a)
= g x a
```

## Curry e Uncurry {.fragile}

Podemos perceber que `leftAdjunct = curry` e `rightAdjunct = uncurry`. 

## Curry e Uncurry {.fragile}

Lembrando que `(,a) = Writer a` e, partindo da definição de Functors Adjuntos, podemos criar dois novos Functors com a composição de `Reader` com `Writer`:

## Curry e Uncurry {.fragile}

```Haskell
-- State a b = Reader a (Writer a b)
type State a b = a -> (b,a)

-- Store a b = Writer (Reader a b) a
type Store a b = (a -> b, a)
```

Esses Functors permitem a criação de estados e armazenamento em uma linguagem puramente funcional. 


