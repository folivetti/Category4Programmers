## Tipo Função {.fragile}

O tipo função, $z$, é encontrado utilizando três objetos:

1. $z$ que representa nosso tipo função (contendo todas as funções $f : a \rightarrow b$)
2. $a$ que representa o tipo do argumento da função
3. $b$ que representa o resultado da aplicação da função

## Tipo Função {.fragile}

\centering
\begin{tikzpicture}[auto, scale=2, transform shape]
\tikzset{my node/.style={node distance=0.5cm}}
\node[draw, circle, label={[black]135:z}, minimum size=0.8cm] (z) {$f$};
\node[draw, circle, label={[black]215:a}, minimum size=0.8cm] (a) [below of=z] {$x$};
\node[draw, circle, label={[black]325:b}, minimum size=0.8cm] (b) [right of=a] {$f x$};
\end{tikzpicture}

## Tipo Função {.fragile}

A conexão desses três objetos é conhecida pelos programadores como *aplicação* ou *avaliação* de função. 

Dado um tipo função $z$ e um tipo de entrada $a$, o morfismo mapeia essa tupla em um tipo $b$. Ou seja, temos um tipo produto formado por $(z, a)$ e um morfismo `g :: (z, a) -> b`.

## Tipo Função {.fragile}

\centering
\begin{tikzpicture}[auto, scale=2, transform shape]
\node (c) {$c$};
\node (z) [below left of=c] {$z$};
\node (a) [below right of=c] {$a$};
\node (b) [right of=c] {$b$};
\draw[->] (c) edge node[above] {} (z);
\draw[->] (c) edge node {} (a);
\draw[->] (c) edge node {$g$} (b);
\end{tikzpicture}

## Tipo Função {.fragile}

Um $z$ é melhor que um $z'$ caso exista um morfismo `h :: z' -> z` que fatora $g'$ em $g$, ou seja, podemos definir $g'$ em função de $h$ e $g$.

## Tipo Função {.fragile}

Como $g, g'$ recebe e retorna uma tupla, utilizamos um Bifunctor `(h, id)` para determinar:

```haskell
g' = g . (h, id)
```

## Tipo Função {.fragile}

Vamos denominar o melhor $z$ como $a \implies b$ e o $g$ correspondente como `eval`.

## Tipo Função {.fragile}

| Um **objeto função** de `a` para  `b` é denominado $a \implies b$ junto com o morfismo `eval :: ((a => b), a) -> b` tal que para qualquer outro objeto $z$ com um morfismo `g :: (z, a) -> b` existe um único morfismo `h :: z -> (a => b)` que `g = eval . (h, id)`.

## Currying {.fragile}

Dada a escolha de um objeto $z$ acompanhado de seu morfismo $g$. O morfismo pode ser interpretado como uma função de dois argumentos $(z, a)$ que retorna um $b$:

```Haskell
g :: (z, a) -> b
```

## Currying {.fragile}

Sabemos que a melhor escolha de objeto pode ser encontrada através da aplicação de $h$, que transforma nosso $z$ em um `a -> b`:

```Haskell
h :: z -> (a -> b)
```

## Currying {.fragile}

A função $h$ recebe um objeto do tipo $z$ e retorna uma função de $a$ para $b$, é uma função de alta ordem. 


## Currying {.fragile}

Removendo os parênteses temos que `h :: z -> a -> b` é a assinatura de uma função que recebe dois argumentos em Haskell.

Isso é chamado de **currying** e dizemos que `h` é a forma  *curried*  de `g`. 

## Currying {.fragile}

Podemos definir a forma *uncurried* utilizando o morfismo `eval`, que reconstrói nosso `g`:

```Haskell
g = eval . (h, id) :: (z, a) -> b
```


# Monads

### Notação *do* {.fragile}

Relembrando um exemplo inicial da categoria Kleisli, tínhamos:

```Haskell
notW :: Bool -> Writer Bool
notW b = (b, "not")

is_even :: Int -> Writer Bool
is_even x = (x `mod` 2 == 0, "even")

is_odd :: Int -> Writer Bool
is_odd = is_even >=> notW
```

### Notação *do* {.fragile}

O Haskell permite um *syntactic sugar* que transforma essa composição em uma notação similar ao paradigma imperativo:

```Haskell
is_odd x = do 
             ev <- is_even x
             is_odd ev
```

### Notação *do* {.fragile}

Que é *parseado* em:

```Haskell
is_even x >>= \ev -> is_odd ev
```

A notação `a <- f x` é lida como `a` recebe o resultado de `f x` sem a parte embelezada.

### Notação *do* {.fragile}

Para o caso do nosso Monad `Writer w` podemos também embelezar uma função automaticamente durante a notação `do`:

```Haskell
even x = x `mod` 2 == 0

tell :: w -> Writer w ()
tell s = Writer ((), s)

is_odd x = do tell "even"
              ev <- return (even x)
              tell "not"
              return (not ev)
```

### Notação *do* {.fragile}

Isso é traduzido para:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
tell "even" >>= (\() -> return (even x) 
            >>= \ev -> tell "not" 
            >>= \() -> return (not ev))

Writer (f (), "even" ++ w')
Writer (g () (even x), "even" ++ "" + w'')
Writer (h () (even x) (), "even" ++ "" + "not" + w''')
Writer ((not.even) x, "even" ++ "" + "not" + "")
Writer ((not.even) x, "evennot")
```
### Read-only {.fragile}

Utilizando `do`-notation o mesmo programa fica:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
f3 :: [Double] -> Reader Config [Double]
f3 xs = do
  t <- askFor it
  return (go t xs)

f2 :: [Double] -> Reader Config [Double]
f2 xs = do
  t  <- askFor thr
  f3 (filterLess t xs)

algorithm :: [Double] -> Reader Config [Double]
algorithm xs = do
  alg <- askFor alg
  if alg == "f2" 
  then f2 xs 
  else f3 xs
```


### State {.fragile}

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
randomSt :: (RandomGen g) => State g Double
randomSt = state (randomR (0.0, 1.0))

mutation :: [Bool] -> State StdGen [Bool]
-- se a lista estiver vazia, nada a fazer
mutation [] = return []

mutation (b:bs) = do
    -- aplica o algoritmo no restante da lista, 
    -- o estado atual do gerador é passado implicitamente 
    -- para a função
    bs' <- mutation bs

    -- sorteia um valor aleatório e altera de acordo
    p   <- randomSt
    if p < 0.3
    then return (not b : bs')
    else return (b : bs')
```


### Reader Comonad {.fragile}

Relembrando o Reader Monad que definimos no post anterior:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
data Reader e a = Reader (e -> a)

instance Monad (Reader e) where
  -- (Reader e a) -> (a -> Reader e b) -> (Reader e b)
  -- (Reader f) >>= k = \e -> runReader (k (f e)) e
  ra >>= k = Reader (\e -> let a  = runReader ra e
                               rb = k a
                           in  runReader rb e)
                           
  return a = Reader (\e -> a)
```

### Reader Comonad {.fragile}

A versão embelezada dos morfismos na categoria Kleisli é `a -> Reader e b` que pode ser traduzido para `a -> (e -> b)` e colocado na forma *curry* de `(a, e) -> b`. 

### Reader Comonad {.fragile}

Com isso, conseguimos definir o Comonad de `Writer e` como o complemento do Monad Reader:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
instance Comonad (Writer e) where
  (=>=) :: (Writer e a -> b) -> (Writer e b -> c) -> (Writer e a -> c)
  f =>= g = \(Writer e a) -> let b = f (Writer e a)
                                 c = g (Writer e b)
                              in c
                              
  extract (Writer e a) = a
```

### Reader Comonad {.fragile}

Basicamente, a função `extract` ignora o ambiente definido por `e` e retorna o valor `a` contido no container. 

O operador de composição simplesmente pega duas funções que recebem tuplas como parâmetros, sendo a primeira do mesmo tipo, e aplica sequencialmente utilizando o mesmo valor de `e` nas duas chamadas (afinal `e` é *read-only*).

### Definições padrão {.fragile}

Examinando o operador `=>=` temos como argumentos `f :: w a -> b` e `g :: w b -> c` e precisamos gerar uma função `h :: w a -> c`. Para gerar um valor do tipo `c`, dado `f, g`, a única possibilidade é aplicar `g` em um tipo `w b`:

```Haskell
f =>= g = g ...
```

### Definições padrão {.fragile}

Tudo que temos a disposição é uma função `f` que produz um `b`. Precisamos então de uma função com a assinatura `(w a -> b) -> w a -> w b`, que é nossa função `extend` (`=>>`). Com isso temos a definição padrão:

```Haskell
f =>= g = \wa -> g . (f =>>) wa
-- ou
-- f =>= g = g . (f =>>)
```

### Definições padrão {.fragile}

Da mesma forma, pensando no operador `=>>` com assinatura `(w a -> b) -> w a -> w b`, percebemos que não tem como obter diretamente um `w b` ao aplicar a função argumento em `w a`. 

### Definições padrão {.fragile}

Porém, uma vez que `w` necessariamente é um Functor, temos a disposição a função `fmap :: (c -> b) -> w c -> w b`, que ao fazer com que `c = w a`, temos `(w a -> b) -> w (w a) -> w b`. 

Se conseguirmos produzir um `w (w a)`, podemos utilizar `fmap` para implementar `=>>`. 
