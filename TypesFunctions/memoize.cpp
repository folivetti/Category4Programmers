#include <iostream>
#include <map>
#include <ctime>
#include <cstdlib>

/* Problemas de implementação: o cache é compartilhado para todas as memoizaçõe */

using namespace std;

auto const memoize = [](auto f) {
	return [f](unsigned int x) {
		static map<int, int> cache;
		if (cache.find(x)==cache.end()) cache[x] = f(x);
		return cache[x];
	};
};

unsigned int fibo(unsigned int n) {
	if (n==0 || n==1) return n;
	return fibo(n-1) + fibo(n-2);
}

unsigned int report_time(auto f, auto n) {
	time_t t1, t2;
	unsigned int x;

	t1 = time(NULL);
	x = f(n);
	t2 = time(NULL);

	cout << "Result: " << x << " in " << t2-t1 << " secs." << endl;

	return x;
}

unsigned int rand_range(unsigned int x) {
	return rand() % x;
}

unsigned int rand_seed(unsigned int x) {
	srand(x);
	return rand();
}

int main() {

	auto mfibo = memoize(fibo);
	auto mrange = memoize(rand_range);
	auto mseed = memoize(rand_seed);

	report_time(fibo, 45);
	report_time(mfibo, 45);
	report_time(mfibo, 45);

	cout << rand_range(100) << endl;
	cout << rand_range(100) << endl;
	cout << mrange(100) << endl;
	cout << mrange(100) << endl;

	cout << rand_seed(120) << endl;
	cout << rand_seed(120) << endl;
	cout << mseed(120) << endl;
	cout << mseed(120) << endl;
	
	return 0;
}


