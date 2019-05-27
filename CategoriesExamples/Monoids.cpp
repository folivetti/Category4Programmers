/* compile with gcc+8 compiler flags -fconcepts -std=c++2a */
#include <iostream>
#include <string>
#include <cstddef>

template<class T>
  T mempty = delete;

template<class T>
  T mappend(T, T) = delete;

template<class M>
  concept bool Monoid = requires (M m) {
    { mempty<M> } -> M;
    { mappend(m, m); } -> M;
  };
  
template<class T>
  T mempty;

template<class T>
  T mappend(T, T);

template<class M>
  concept bool Monoid = requires (M m) {
    { mempty<M> } -> M;
    { mappend(m, m) } -> M;
  };
  
template<>
int mempty<int> = {1};

int mappend(int x, int y) {
    return x*y;
}

typedef struct produto {
    int qtd;
    double preco;
} produto;

template<>
int mempty<produto> = (produto){0, 0.0};

int mappend(produto x, produto y) {
    produto z = (produto){x.qtd+y.qtd, x.preco+y.preco};
    return z;
}

produto soma_produtos (produto ps[], int n) {
    produto total = mempty<produto>;
    
    for (int i = 0; i<n; i++) {
        mappend(total, ps[i]);
    }
    
    return total;
}

produto zera_estoque (produto p) {
    return mempty<produto>;
}

int main()
{
  std::cout << mappend(2,3);
}
