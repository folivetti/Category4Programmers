#include <iostream>     // std::cout
#include <algorithm>    // std::sort
#include <vector>       // std::vector
#include <string>
#include <functional>

typedef struct person {
  int age;
  std::string name;
} person;

typedef struct employee {
  int id;
  person p;
} employee;

bool sortAge (int i,int j) { return (i<j); }

int getAge(person p) { return p.age; }
person getPerson(employee e) { return e.p; }

auto const compose = [](auto f, auto g) {
        return [f, g](auto x) {
                return g(f(x));
        };
};

template<class A, class B>
auto contramap(std::function<bool(B, B)> f, std::function<B(A)> g)
{
  return [f, g](A x, A y){return f(g(x), g(y));};
}

auto sortEmployee = contramap<employee, int>(sortAge, compose(getPerson, getAge));

int main () {
  person p1 = {23, "Olaf"};
  person p2 = {27, "George"};
  person p3 = {19, "Carl"};

  employee es[] = {{1, p1}, {2, p2}, {3, p3}};

  std::vector<employee> myvector (es, es+3);

  // using function as comp
  std::sort (myvector.begin(), myvector.end(), sortEmployee);

  // print out content:
  std::cout << "myvector contains:";
  for (std::vector<employee>::iterator it=myvector.begin(); it!=myvector.end(); ++it)
    std::cout << ' ' << (it->p).name << ' ' << (it->p).age;
  std::cout << '\n';

  return 0;
}
