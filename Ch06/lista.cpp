#include <iostream>

using namespace std;

template<class A>
class List;

template<class A>
class Node {
    A value;
    List<A> * next;
public:    
    Node(A a, List<A> &l) {value=a; next=l;};
};

template<class A>
class List {
    Node<A> * _head;
public:
    List() : _head(nullptr) {}  // Nil
    List(A a, List<A> l)        // Cons
      : _head(new Node<A>(a, l))
    {}
};
