#include <iostream>
#include <cmath>
#include <tuple>
#include <functional>
#include <experimental/optional>
#include <string>

using namespace std;
using namespace experimental;


int dobra(int x){ return 2*x; }
bool intbool(int x){ return (x>3); }

template <class F, class G>
auto fmap(F f, G g){
  return [f,g](auto x){return f(g(x));};
}


int main() {
    int y=2;
    bool r = fmap(intbool, dobra)(y);
    cout << r << endl;
}
