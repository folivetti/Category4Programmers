#include <iostream>

using namespace std;

template<class A>
  using Writer = pair<A, string>;

template<class A>
Writer<A> identity(A x) {
    return make_pair(x, "");
}

auto const compose = [](auto m1, auto m2) {
    return [m1, m2](auto a) {
        auto p1 = m1(a);
        auto p2 = m2(p1.first);
        return make_pair(p2.first, p1.second + p2.second);
    };
};

Writer<bool> notW(bool b) {
    return make_pair(!b, "not ");
}

Writer<bool> is_even(int x) {
    return make_pair(x%2==0, "even ");
}

Writer<bool> is_odd(int x) {
    return compose(is_even, notW)(x);
}

int main() {
    auto res = is_odd(7);
    cout << res.first << " " << res.second << endl;
}
