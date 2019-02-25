#include <iostream>
#include <string>
#include <cstddef>

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

int main()
{
  std::cout << mappend(2,3);
}
