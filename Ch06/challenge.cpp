#include <iostream>

#define PI 3.1415

using namespace std;

class Shape {
	public:
		virtual double area() = 0;
		virtual double circ() = 0;
};

class Circle : public Shape {
	public:
		double area() { return PI*r*r; }
		double circ() { return 2.0*PI*r; }
		void set_radius(double radius) {r=radius;};
	protected:
		double r;
};

class Rect : public Shape {
	public:
		double area() { return w*h; }
		double circ() { return 2.0*(w+h); }
		void set_sides(double wi, double hi) {w=wi; h=hi;};
	protected:
		double w;
		double h;
};

class Square : public Shape {
	public:
		double area() { return s*s; }
		double circ() { return 4.0*s; }
		void set_side(double si) {s=si;};
	protected:
		double s;
};

int main() {
	Circle c;
	c.set_radius(2.0);
	cout << c.area() << endl;

	return 0;
}
