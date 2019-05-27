#include <iostream>

using namespace std;

template<class A>
class List;

template<class A>
struct Node {
    Node(A a, List<A> l) {value=a; next=l;};
    A value;
    List<A> next;
};

template<class A>
class List {
    Node<A> * _head;
public:
    List() : _head(nullptr) {}  // Nil
    List(A a, List<A> l)        // Cons
      : _head(new Node<A>(a, l))
    {}
   
    Node<A> * notEmpty() {return _head;} 
    A head() {return _head->value;}
    List<A> * tail() {return _head ? &(_head->next) : nullptr;};
};

int main() {
	List<int> l = List<int>(2, List<int>(5, List<int>(3, List<int>())));
	List<int> * walker = &l;

	while (walker->notEmpty()) {
		cout << walker->head() << endl;
		walker = walker->tail();
	}

	
	return 0;
}
