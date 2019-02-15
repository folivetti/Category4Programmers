#include <iostream>
#include <cmath>
#include <tuple>
#include <experimental/optional>

using namespace std;
using namespace experimental;

optional<double> safe_root(double x) {
    if (x >= 0) return optional<double>{sqrt(x)};
    else return optional<double>{};
}

optional<double> safe_reciprocal(double x) {
	if (x != 0) return optional<double>{1.0/x};
	else return optional<double>{};
}

auto pure(double x) {
	return optional<double>{x};
}

auto const fish = [](auto f, auto g) {
	return [f, g](double x) {
		auto z = f(x);
		if (z) return g(z.value());
		else return z;
	};
};

void print_safe(auto x) {
	if (x) cout << x.value() << endl;
	else cout << "Nada" << endl;
}

int main() {
	optional<double> x;

	x = safe_root(16);
	print_safe(x);

	x = safe_root(-16);
	print_safe(x);

	auto c = fish(safe_reciprocal, safe_root);
	x = c(2);
	print_safe(x);

	x = c(0);
	print_safe(x);

	x = c(-1);
	print_safe(x);

	return 0;
}
