---
title: "Teoria das Categorias para Programadores"
author: Fabrício Olivetti de França
date: 10 de Agosto de 2019
mainfont: DejaVu Sans
fontsize: 12pt
---

# Functors

## Functors

Um outro conceito de Teoria das Categorias que pode ser diretamente relacionado com programação é o **Functor**.

## Functors

**Functor** é um mapa de objetos e morfismos de uma categoria $C$ para uma categoria $D$:


$F : C \rightarrow D$ é um functor de $C$ para $D$ se $a, b, f \in C$, sendo $f : a \rightarrow b$ então $F a, F b, F f \in D$ e $F f : F a \rightarrow F b$.

## Functors

\centering
\begin{tikzpicture}[auto, scale=2, transform shape]
\node (a) {$a$};
\node (b) [right of=a, xshift=10mm] {$b$};
\node (Fa) [above of=a, yshift=2mm] {$F a$};
\node (Fb) [above of=b, yshift=2mm] {$F b$};
\draw[->] (Fa) edge[bend left=90] node (Ff) {$F f$} (Fb);
\draw[->] (a) edge[bend left=-90] node (ff) [below]{$f$} (b);
\draw[dashed, ->] (a) edge (Fa);
\draw[dashed, ->] (b) edge (Fb);
\draw[dashed, ->] (ff) edge node {$F$} (Ff);
\end{tikzpicture}

## Functors

Esse mapa não apenas transforma uma categoria em outra, mas também preserva sua estrutura, ou seja, tanto os morfismos identidades como as composições são mantidas intactas:

$F id_{a} = id_{F a}$

e

$h = g . f \implies F h = F g . F f$

## Functors em Linguagem de Programação

Pensando na categoria dos tipos, temos na verdade **endofunctors** que mapeiam a categoria dos tipos para ela mesma, ou seja.

## Functors em Linguagem de Programação

Podemos pensar em um Functor $F$ como um tipo paramétrico, ele é capaz de pegar qualquer tipo $a$ de nossa categoria e criar um tipo $F a$ que **contém** valores de $a$.

Em outra palavra é um **container**.

## Containers

Um exemplo de container é uma lista, podemos ter uma lista de `Int`, lista de `Char`, etc.

Quais outros containers vocês conhecem?

## Functor Lista

Vamos revisar a definição de uma lista em Haskell:

```Haskell
data List a = Empty | a : (List a)
```

Uma lista do tipo `a` ou é vazia (`Empty`) ou tem um elemento do tipo `a` seguido por outra lista do mesmo tipo. 

## Functor Lista

Pensando que um Functor é um container, então poderíamos dizer que $F = List$. 

Porém, um Functor deve manter toda a estrutura do tipo contido na lista. Ou seja, para qualquer $f : a \rightarrow b$, devo ter um $F f : F a \rightarrow F b$.

## Functor Lista {.fragile}

Para termos um Functor precisamos ter um mapa de morfismos. A definição de um Functor em Haskell evidencia isso:

```Haskell
class Functor F where
    fmap :: (a -> b) -> (F a -> F b)
```

`fmap` recebe uma função de `a` para `b` e retorna uma função de `F a` para `F b`. Isso é chamado de **lift**.

## Functor Lista {.fragile}

Para simplificar, podemos remover o segundo par de parênteses e ler de outra forma:

```Haskell
class Functor F where
    fmap :: (a -> b) -> F a -> F b
```

Dada uma função de `a` para `b` e um Functor de `a`, eu retorno um Functor de `b`.

## Functor Lista

Diante dessa segunda leitura, como você implementaria a função `fmap` para listas? (em qualquer linguagem)

## Functor Lista {.fragile}

Em Haskell temos:

```Haskell
instance Functor List where
    fmap f []     = []
    fmap f (x:xs) = f x : fmap f xs
```

## Functor Lista {.fragile}

Aplicando no seguinte exemplo temos:

```Haskell
let xs = 1 : 2 : 3 : []
fmap show xs
-- fmap show (1:xs) = show 1 : fmap show xs
-- = show 1 : fmap show (2:xs)
-- = show 1 : show 2 : fmap show xs
-- = show 1 : show 2 : show 3 : fmap show []
-- = show 1 : show 2 : show 3 : []
```

## Tudo é um Functor

Discutimos alguns exemplos de containers...mas quais os containers mais simples que vocês conseguem imaginar?

## Const Functor {.fragile}

O mais simples é aquele que não armazena nada! Ele é conhecido como `Const Functor` e simplesmente guarda um mesmo valor **sempre**:

```Haskell
data Const b a = Const b
```

Como você implementaria `fmap` para esse Functor?

## Const Functor {.fragile}

```Haskell
instance Functor (Const b) where
    fmap _ (Const x) = Const x
```

Ele será útil para automatizarmos a tarefa de construir um Functor!

## Identity Functor {.fragile}

O segundo Functor mais simples é aquele que guarda um único valor do tipo `a`:

```Haskell
data Identity a = Identity a
```

Como você implementaria `fmap` para esse Functor?

## Identity Functor {.fragile}

```Haskell
instance Functor Identity where
    fmap f (Identity x) = Identity (f x)
```

O Functor `Identity a` é isomorfo ao tipo `a`. Podemos então dizer que todo tipo é um Functor!

## Construindo novos Functors

Temos um Functor que descarta informação (`Const`) e outro que guarda um único valor (`Identity`). Como construímos um container que **ou** guarda nada ou guarda apenas um valor?

## Construindo novos Functors {.fragile}

```Haskell
data NadaOuUm a = Either (Const () a) (Identity a)
```

Como você definiria a função `fmap`?

## Construindo novos Functors {.fragile}

```Haskell
instance Functor NadaOuUm where
    fmap _ (Left Const ()) = Left Const ()
    fmap f (Right Identity x) = Right Identity (f x)
```

## Construindo novos Functors

Esse tipo `NadaOuUm` é isomorfo a qual outro tipo que vimos anteriormente?

## Construindo novos Functors {.fragile}

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
f :: Maybe a -> NadaOuUm a
f Nothing  = Const ()
f (Just x) = Identity x

g :: NadaOuUm a -> Maybe a
g (Left (Const ()))    = Nothing
g (Right (Identity x)) = Just x

f . g = id
g . f = id
```

## Functor Maybe {.fragile}

```Haskell
instance Functor Maybe where
    -- fmap _ (Const ()) = Const ()
    fmap _ Nothing = Nothing
    
    -- fmap f (Identity x) = Identity (f x)
    fmap f (Just x) = Just (f x)
```

## É realmente um Functor? {.fragile}

Precisamos verificar se nossa definição obedece as propriedades de um Functor:

```Haskell
fmap id = id
fmap (g . f) = fmap g . fmap f
```

## É realmente um Functor? {.fragile}

```Haskell
fmap id Nothing = id Nothing
Nothing = Nothing

fmap id (Just x) = id (Just x)
Just (id x) = Just x
Just x = Just x
```

## É realmente um Functor? {.fragile}

```Haskell
fmap (g . f) Nothing = (fmap g . fmap f) Nothing
Nothing = fmap g (fmap f Nothing)
Nothing = fmap g Nothing
Nothing = Nothing
```

## É realmente um Functor? {.fragile}

```Haskell
fmap (g . f) (Just x) = (fmap g . fmap f) (Just x)
Just ((g . f) x) = fmap g (fmap f (Just x))
Just (g (f x)) = fmap g (Just (f x))
Just (g (f x)) = Just (g (f x))
```

## Functor Writer {.fragile}

Relembrando a definição de `Writer` (um pouco diferente da aula anterior):

```Haskell
data Writer s a = Writer a s
```

Como reescrever utilizando `Const` e `Identity`?

## Functor Writer {.fragile}

```Haskell
type Writer s a = (Identiy a, Const s a)
```

Como escrevemos a definição de `fmap` para esse tipo?

## Functor Writer {.fragile}

```Haskell
instance Functor (Writer s) where
    fmap f (Writer x s) = Writer (f x) s
```

## Functor via compilador {.fragile}

A construção de um Functor é um processo mecânico, podemos derivar automaticamente pelo compilador. No compilador `ghc` do Haskell podemos fazer:

```Haskell
{-# LANGUAGE DeriveFunctor #-}

data Maybe a = Nothing | Just a 
  deriving Functor
```

## Functor via compilador {.fragile}

Essa construção funciona pois os Functors podem ser compostos:

```Haskell
instance Functor Maybe where
    fmap f (Left x)  = fmap f x
    fmap f (Right y) = fmap f y
```

Ou seja, definimos o `fmap` em função de `fmap` de outros Functors.

## Functor Reader {.fragile}

Um outro container interessante é o `Reader`, que é representado por uma função:

```Haskell
type Reader r a = r -> a
```

## Functor Reader {.fragile}

Dado um `Reader r a` e uma função `a -> b` o `fmap` deve criar um `Reader r b`. 

## Functor Reader {.fragile}

Dado um `r -> a` e uma função `a -> b` o `fmap` deve criar um `r -> b`. 

## Functor Reader {.fragile}

```Haskell
instance Functor (Reader r) where
    -- fmap :: (Reader r a) -> (a -> b) -> (Reader r b)
    -- fmap :: (r -> a) -> (a -> b) -> (r -> b)
    fmap = ???
```

## Functor Reader {.fragile}

```Haskell
instance Functor (Reader r) where
    -- fmap :: (Reader r a) -> (a -> b) -> (Reader r b)
    -- fmap :: (r -> a) -> (a -> b) -> (r -> b)
    fmap = (.)
```

## Functor Reader {.fragile}

Se funções são Functors, funções podem ser interpretadas como containers!

Concordam??

## Functor Reader {.fragile}

Funções puras podem ser memoizadas, ou seja, ter seus resultados armazenados em um container. 

O inverso também é válido, um container pode ser representado como uma função.

## Functor Reader {.fragile}

Com essa intuição, podemos definir tipos infinitos (ex.: *stream* de dados):

```Haskell
-- lista infinita com os números naturais
nat = [1..]

nat = 1 : fmap (+1) nat
-- 1 : (+1) 1 : (+1) (+1) 1 : ...
```

## Composição de Functors {.fragile}

Podemos compor dois ou mais Functors criando estruturas mais complexas de forma simplificada graças as propriedades do Functor:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (x:xs) = Just xs

square :: Integer -> Integer
square x = x*x

xs :: [Integer]
xs = [1 .. 10]

fmap (fmap square) (maybeTail xs)
=
(fmap . fmap) square (maybeTail xs)
```

## Functors em outras linguagens {.fragile}

A definição de `fmap` em C++ para o tipo `optional` pode ser escrita como:

```{.cpp frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
template<class A, class B>
std::optional<B> fmap(std::function<B(A)> f, std::optional<A> opt) 
{
    if (!opt.has_value())
        return std::optional<B>{};
    else
        return std::optional<B>{ f(*opt) };
}
```

## Functors em outras linguagens {.fragile}

E para o tipo `vector`:

```{.cpp frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
template<class A, class B>
std::vector<B> fmap(std::function<B(A)> f, std::vector<A> v) 
{
   std::vector<B> w; 
   std::transform(std::begin(v), std::end(v), std::back_inserter(w) , f);
   return w;
}
```

## Functors em outras linguagens {.fragile}

```{.cpp frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
int dobra(int x) {
  return 2*x;
}

int main()
{
  std::optional<int> o1, o2;
  std::function<int(int)> f = dobra;
  
  std::vector<int>  v{ 1, 2, 3, 4 };
  
  o1 = {3};
  o2 = {};
  
  std::cout << fmap(f, o1).value_or(-1) << std::endl;
  std::cout << fmap(f, o2).value_or(-1) << std::endl;
  for (auto const& c : fmap(f, v))
    std::cout << c << ' ';
  std::cout << std::endl;
  
  return 0;
}

```

## Functors em outras linguagens {.fragile}

Em Python temos que usar `singledispatch` com os parâmetros invertidos, pois o tipo paramétrico deve ser o primeiro parâmetro:

```{.python frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
class Maybe:
    def __init__(self, x = None):
        self.val = x

@singledispatch
def fmap(a, f):
    print("Not implemented for" + str(type(a)))
```

## Functors em outras linguagens {.fragile}

```{.python frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
@fmap.register(list)
def _(l, f):
    return list(map(f, l))
    
@fmap.register(Maybe)
def _(m, f):
    if m.val is None:
        m.val = None
    else:
        m.val = f(m.val)
    return m
```

## Functors em outras linguagens {.fragile}

```{.python frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
f = lambda x: x*2

l = [1,2,3]
m1 = Maybe(2)
m2 = Maybe()

print(fmap(l, f))
print(fmap(m1, f).val)
print(fmap(m2, f).val)
```

# Profunctors e Bifunctors

## Bifunctors {.fragile}

Vimos anteriormente que muitos Functors são aplicados em tipos paramétricos com **dois** parâmetros. Por exemplo, temos os dois tipos algébricos fundamentais: `Either a b` e `Pair a b`.

## Bifunctors {.fragile}

Nesses casos devemos decidir qual parâmetro fica fixo e em qual aplicamos a função. Convencionamos de fixar o primeiro dos tipos paramétricos.

Por que não criar um Functor que permite aplicar funções em ambos os parâmetros?

## Bifunctors {.fragile}

```Haskell
class Bifunctor f where
    bimap :: (a -> c) -> (b -> d) -> (f a b -> f c d)
```

## Bifunctors {.fragile}

Podemos definir um Bifunctor para os tipos `Either` e para tuplas como:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
instance Bifunctor Either where
    bimap f _ (Left x)  = Left (f x)
    bimap _ g (Right y) = Right (g y)
    
instance Bifunctor (,) where
    bimap f g (x, y) = (f x, g y)
```

## Bifunctors {.fragile}

No Haskell o Bifunctor também pode ser definido através das funções `first` e `second` com implementações padrão:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
class Bifunctor f where
    bimap :: (a -> c) -> (b -> d) -> (f a b -> f c d)
    bimap f g = first f . second g
    
    first :: (a -> c) -> (f a b -> f c b)
    first f = bimap f id
    
    second :: (b -> d) -> (f a b -> f a d)
    second g = bimap id g
```

## Bifunctors {.fragile}

Para os tipos `Either` e `Pair` essas funções ficariam:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
instance Bifunctor Either where
    first f (Left x)  = Left (f x)
    first _ (Right y) = Right y
    
    second _ (Left x)  = Left x
    second g (Right y) = Right (g y)
    
instance Bifunctor (,) where
    first f (x, y)  = (f x, y)
    second g (x, y) = (x, g y)
```

## Functors Covariantes e Contravariantes {.fragile}

Os Functors que vimos até então tem um nome mais específico, eles são chamados de **Covariantes**. Lembrando do Functor `Reader` definido como:

```Haskell
type Reader r a = r -> a

instance Functor (Reader r) where
  fmap f g = f . g
```

## Functors Covariantes e Contravariantes {.fragile}

Se quiséssemos definir um Bifunctor para o tipo função, teríamos que definir primeiro um Functor para o tipo `Op`:

```Haskell
type Op r a = a -> r

instance Functor (Op r) where
  fmap :: (a -> b) -> (Op r a) -> (Op r b)
=
  fmap :: (a -> b) -> (a -> r) -> (b -> r)
```

Não tem como definirmos uma função `fmap` com essa assinatura! 

## Functors Covariantes e Contravariantes {.fragile}

Precisamos de um argumento do tipo `b -> a`. Isso é definido pelo Functor **Contravariante** da categoria oposta ao Covariante:

```Haskell
class Contravariant f where
    contramap :: (b -> a) -> (f a -> f b)
    
instance Contravariant (Op r) where
    -- (b -> a) -> Op r a -> Op r b
    contramap f g = g . f
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

Imagine que temos as seguintes estruturas de dados:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
data Person   = Person   {name :: String
                         , age :: Int
                         }
                         
data Employee = Employee {tag :: Int
                         , person :: Person
                         , salary :: Double
                         }
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

Dada a função `sortBy :: (a -> a -> Ordering) -> [a] -> [a]`, podemos fazer:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
cmpAge :: Int -> Int -> Ordering
cmpAge x y | x < y  = LT
           | x > y  = GT
           | x == y = EQ
           
cmpPerson :: Person -> Person -> Ordering
cmpPerson x y = cmpAge (age x) (age y)

cmpEmployee :: Employee -> Employee -> Ordering
cmpEmployee x y = cmpAge ((age. person) x) ((age.person) y)
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

Observando essas funções, percebemos um padrão de repetição em nossos códigos.Idealmente poderíamos ter uma função `f` que aplica uma função em nossos registros antes de aplicar a função de comparação:

```{.haskell frame=lines framerule=2pt linenos=true fontsize=\footnotesize baselinestretch=0.8}
cmpPerson   = f age cmpAge
cmpEmployee = f (age.person) cmpAge
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

Qual deve ser a assinatura dessa função?

```Haskell
f :: (?? -> ??) -> (Int -> Int -> Ordering) -> (?? -> ?? -> Ordering)
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

Vamos generalizar o tipo `Int` em um tipo genérico `a`:

```Haskell
f :: (?? -> ??) -> (a -> a -> Ordering) -> (?? -> ?? -> Ordering)
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

O primeiro argumento recebe tipo `b` e transforma em um tipo `a`, pois sabemos ordenar o tipo `a`:

```Haskell
f :: (b -> a) -> (a -> a -> Ordering) -> (?? -> ?? -> Ordering)
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

Finalmente, podemos criar uma função que sabe ordenar o tipo `b`:

```Haskell
f :: (b -> a) -> (a -> a -> Ordering) -> (b -> b -> Ordering)
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

Digamos que a função de comparação é um Functor do tipo `Order a`, com isso temos:

```Haskell
f :: (b -> a) -> Order a -> Order b
```

Já vimos algo parecido com isso...

## Exemplo de aplicação: Composição de Comparadores {.fragile}

```Haskell
contramap :: (b -> a) -> F a -> F b
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

```Haskell
type Order a = a -> a -> Ordering

instance Contravariant Order where
  --contramap :: (b -> a) -> (a -> a -> c) -> (b -> b-> c)
  contramap f c = \x y -> c (f x) (f y)
```

## Exemplo de aplicação: Composição de Comparadores {.fragile}

Com isso nossa ordenação fica:

```Haskell
cmpPerson'   = contramap age cmpAge
cmpEmployee' = contramap (age.person) cmpAge
```

# Profunctors

## Profunctors {.fragile}

A classe de um Bifunctor composto de dois Functors cujo primeiro é Contravariante e o segundo Covariante, é chamada de **Profunctor**:

```Haskell
class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  dimap f g = lmap f . rmap g
  lmap :: (a -> b) -> p b c -> p a c
  lmap f = dimap f id
  rmap :: (b -> c) -> p a b -> p a c
  rmap = dimap id
```

## Profunctors {.fragile}

Dessa forma podemos definir o operador função `(->)` como um Profunctor (compondo os Functors `Op` e `Reader`):

```Haskell
instance Profunctor (->) where
  lmap f g = g . f
  rmap f g = f . g
```

A definição de Profunctors é utilizada na criação de *Lens*, que será abordada em outro curso.

# Tipo Função

## Tipo Função {.fragile}

Em Teoria das Categorias, definimos $C(a, b)$ como o conjunto de morfismos iniciando em $a$ e terminando em $b$.

Na categoria dos tipos um morfismo é uma função que recebe um argumento do tipo $a$ e retorna um tipo $b$.

Se $a \rightarrow b$ representa o conjunto de funções com essa assinatura, podemos definir um Tipo Função.

## Tipo Função {.fragile}

Vamos utilizar a construção universal!!

O padrão que queremos é formado por três objetos:

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

Um $z$ é melhor que um $z'$ caso exista um morfismo `h :: z' -> z` que fatora $g'$ em $g$, ou seja, podemos definir $g'$ em função de $h$.

## Tipo Função {.fragile}

A fatoração recebe um par $(z', a)$ e transforma em um par $(z, a)$. 

A transformação de um tipo produto `(a, b)` para um tipo `(c, d)` é feito através de um Bifunctor:

```haskell
bimap = (h, id) :: (z', a) -> (z, a)`
```

Com isso conseguimos definir `g' = g . (h, id)`.

## Tipo Função {.fragile}

Finalmente vamos encontrar o melhor entre todos os candidatos de $z$. Vamos chamar o melhor $z$ como $a \implies b$ e o melhor $g$ como `eval`:

| Um **objeto função** de `a` para  `b` é denominado $a \implies b$ junto com o morfismo `eval :: ((a => b), a) -> b` tal que para qualquer outro objeto $z$ com um morfismo `g :: (z, a) -> b` existe um único morfismo `h :: z -> (a => b)` que `g = eval . (h, id)`.

# Currying

Imagine que escolhemos um certo objeto $z$ acompanhado de seu morfismo $g$. O morfismo pode ser interepretado como uma função de dois argumentos $(z, a)$ que retorna um $b$:

```Haskell
g :: (z, a) -> b
```

Sabemos que a melhor escolha de objeto pode ser encontrada através da aplicação de $h$, que transforma nosso $z$ em um `a -> b`:

```Haskell
h :: z -> (a -> b)
```

Dizemos que $h$ é uma função que recebe um objeto do tipo $z$ e retorna uma função de $a$ para $b$, é uma função de alta ordem. Isso nos diz que toda função de dois argumentos é, na verdade, uma função de um argumento que retorna outra função. Isso é conhecido como **currying**, dizemos que `h` é a forma  *curried*  de `g`. Podemos definir a forma *uncurried* utilizando nosso morfismo `eval`, que reconstrói nosso `g`:

```Haskell
g = eval . (h, id) :: (z, a) -> b
```

Ou seja, essas definições são isomórficas. Em Haskell todas as versões de múltiplos argumentos são naturalmente interpretadas como sua versão *curry*:

```Haskell
a -> (b -> c) = a -> b -> c
```

Isso fica claro na definição de uma função de duas variáveis em Haskell:

```Haskell
mult :: Int -> Int -> Int
mult x y = x*y

mult' :: Int -> (Int -> Int)
mult' x = \y -> x*y
```

Que se torna evidente quando fazemos uma aplicação parcial da primeira função:

```Haskell
dobra = mult 2

dobra :: Int -> Int
```

A biblioteca padrão do Haskell já tem a conversão das duas formas de definição de funções:

```Haskell
curry :: ((a, b)->c) -> (a->b->c)
curry f a b = f (a, b)

uncurry :: (a->b->c) -> ((a, b)->c)
uncurry f (a, b) = f a b
```

Em C++ você pode fazer uma aplicação parcial de função utilizando o template `std::bind`:

```C++
int mult(int x, int y) {
    return x*y;
}

using namespace std::placeholders;

auto dobra = std::bind(mult, 2, _1);
std::cout << dobra(4);
```

Em Python temos a função `partial`:

```Python
from functools import partial

def mult(x,y):
    return x * y

dobra = partial(mult, 2)
```

# Tipo de Dados Algébrico Exponencial

A interpretação algébrica de um tipo função é a de um exponencial. Isso se torna evidente com uma função do tipo `Bool` para um certo tipo `a` e outra função de um tipo `a` para `Bool`:

```Haskell
f :: Bool -> a

g :: a -> Bool
```

De quantas maneiras podemos definir a função `f`? A entrada é definida pelos dois possíveis valores `True` e `False`, ou seja, podemos dizer que uma função de `Bool` para `a` é definida por um par de valores de `a`. Já vimos isso anteriormente quando percebemos que `Bool -> a` é isomórfica a `(a, a)`. Com isso temos `a^2` definições diferentes para essa função.

A função `g` deve definir um valor `True` ou `False` para cada um dos valores de `a`, ou seja, é uma tupla `(Bool, Bool, Bool, ..., Bool)` com `a` elementos. Analogamente, isso é equivalente a `2^a` possíveis definições.

O tipo função `a -> b` é representada por uma exponencial `b^a`, indicando as possíveis combinações de entrada e saída para essa assinatura de função.

Vamos verificar se esse tipo também obedece as propriedades algébricas de uma exponenciação.

## Potência de Zero

Uma função $a^0 = 1$ tem a assinatura:

```Haskell
f :: Void -> a
```

que já vimos possuir apenas uma definição que é a função `absurd`.

## Potência envolvendo Um

Analogamente, uma função $a^1 = a$ tem a assinatura:

```Haskell
f :: () -> a
```

que é a função `unit` que seleciona um valor de `a`, portanto possui `a` definições diferentes.

Já a função $1^a = 1$ tem como assinatura e única definição:

```Haskell
const :: a -> ()
const x = ()
```

## Somas de Exponenciais

Uma função $a^{b+c}$ é uma função de um tipo soma para um tipo `a`:

```Haskell
f :: Either b c -> a
f (Left x)  = ...
f (Right y) = ...
```

Ou seja, é uma função que deve ser definida para os casos `b -> a` e `c -> a`. Em outras palavras devemos definir um par de funções, o que é compatível com a propriedade da exponenciação $a^{b+c} = a^b \cdot a^c$.

## Exponenciais de Exponenciais

Uma função $(a^b)^c$ é interpretada como uma função que recebe um tipo `c` e retorna uma função de `b -> a`, ou seja, uma função de alta ordem:

```Haskell
f :: c -> (b -> a)
```

Mas sabemos que essa forma é equivalente a uma função que recebe um par `(c, b)` e retorna um `a`. Ou seja, $(a^b)^c = a^{(bc)}$

## Exponenciais sobre produtos

Também podemos ter uma função $(a \cdot b)^c$ que é representada por:

```Haskell
f :: c -> (a, b)
```

Isso é equivalente a um par de funções `c -> a` e `c -> b` (nossas funções `p, q`  do tipo produto) que nos dá  $(a \cdot b)^c = a^c \cdot b^c$

# Cartesian Closed Category

Uma Categoria que possui:

1. Objeto terminal
2. qualquer $a, b \in C$ implica em $a \cdot b \in C$ (a categoria possui tipo produto)
3. qualquer $a, b \in C$ implica em $a^b \in C$

Dizemos que é uma Categoria Cartesiana Fechada (CCC). Se além disso ela tiver as propriedades complementares:

1. Objeto inicial
2. qualquer $a, b \in C$ implica em $a+b \in C$

Então ela é uma Categoria Bicartesiana Fechada (Bicartesian Closed Category - BCCC).

# Isomorfismo de Curry Howard

Complementando nossa tabela do isomorfismo de Curry Howard temos:

| Algebra     | Tipos                        | Lógica         |
|-------------|------------------------------|----------------|
| $0$         | `Void`                       | Falso          |
| $1$         | `()`                         | Verdadeiro     |
| $a + b$     | Either a b                   | $a \lor b$     |
| $a * b$     | (a, b)                       | $a \land b$    |
| $b^a$       | $a \rightarrow b$            | $a \implies b$ |

A definição de uma função é isomórfica a definição de uma implicação. Por examploe, nossa função `eval` com assinatura:

```Haskell
eval :: ((a->b), a) -> b
```

Pode ser traduzida como: "Se `b` segue de `a` e `a` é verdadeiro, então `b` é verdadeiro.". Provamos essa proposição implementando, ou seja, mostrando que esse tipo é habitável por algum valor:

```Haskell
eval :: ((a->b), a) -> b
eval (f, x) = f x
```

Vamos tentar provar a proposição $(a \lor b) \implies a$, ou seja, se $a$ ou $b$ forem verdadeiros, então $a$ é verdadeiro:

```Haskell
f :: Either a b -> a
f (Left x)  = x
f (Right y) = ???
```

Na segunda definição eu tenho que prover uma expressão que funcione para qualquer que seja o tipo $a$. Impossível!

Podemos então usar funções definidas em Haskell como provas de proposições lógicas.


# Transformação Natural

# Limites e Colimites

# Monoids Livres


