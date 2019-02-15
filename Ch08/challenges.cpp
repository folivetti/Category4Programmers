#include <iostream>
#include <cmath>
#include <tuple>
#include <functional>
#include <experimental/optional>
#include <string>

using namespace std;
using namespace experimental;


template <class A, class B, class C, class D>
pair<C, D> bimap(auto f, auto g, pair<A, B> p){ 
  return pair<C, D>(f(p.first), g(p.second));
}


template <class A>
auto fmap(auto f, A x) {
    return f(x);
}

string int2str(int x) {
    return "s" + to_string(x);    
}

int str2int(string s) {
    return stoi(s,nullptr) + 1;
}

int main() {
    pair<int, string> p;
    p.first = 23;
    p.second = "12";
    
    pair<string, int> p2 = bimap<int, string, string, int>(int2str, str2int, p);
    cout << fmap(int2str, 23) << endl;
    cout << p2.first << " " << p2.second << endl;
    
    return 0;
}
