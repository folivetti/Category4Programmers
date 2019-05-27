#include <iostream>
#include <vector>
#include <experimental/generator>

/* This source code will compile in the future!!! */
//using namespace std::experimental;
using namespace std;

template<typename Src, typename T = typename Src::value_type>
static generator<T> make_generator(const Src& src)
{
	for (const T& t : src) {
		__yield_value t;
	}
}
	
template<typename T, typename Fun>
static generator<typename std::result_of<Fun(T)>::type>
bind(generator<T> as, Fun k)
{
    for (auto a : as) {
        for (auto x : k(a) {
            __yield_value x;
        }
    }
}

vector<int> dobra(int x) {
    return vector<int>{2*x};
}

int main () {

    vector<int> xs = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    auto ys = bind(make_generator(xs), dobra);    
    return 0;
}
