#include <iostream>

using namespace std;

int id(int x) { return x; }

int times2(int x) {
    return 2*x;
}

auto const compose = [](auto f, auto g) {
	return [f, g](auto x) {
		return g(f(x));
	};
};

int main () {
    int x;
    char failed1 = 0, failed2 = 0;

    for (x=0; x<100; x++) 
    {
        if (compose(id, times2)(x) != times2(x)) failed1 = 1;
        if (compose(times2, id)(x) != times2(x)) failed2 = 1;
    }

    if (failed1) cout << "\"Falhou id o times2\"\n";
    else cout << "Passou \"id o times2\"\n";

    if (failed2) cout << "Falhou \"times2 o id\"\n";
    else cout << "Passou \"times2 o id\"\n";

    return 0;
}
