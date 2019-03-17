#include <iostream>
#include <string>
#include <cstddef>
#include <optional>
#include <functional>
#include <vector>

template<class A, class B>
std::optional<B> fmap(std::function<B(A)> f, std::optional<A> opt) 
{
    if (!opt.has_value())
        return std::optional<B>{};
    else
        return std::optional<B>{ f(*opt) };
}

template<class A, class B>
std::vector<B> fmap(std::function<B(A)> f, std::vector<A> v) 
{
   std::vector<B> w; 
   std::transform(std::begin(v), std::end(v), std::back_inserter(w) , f);
   return w;
}

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

