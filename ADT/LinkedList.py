class List:
    def __init__(self, x=None, l=None):
        if x is None:
            self.head = None
        self.head = (x, l)

lista = List(1, List(2, List(3, List(None))))

while lista is not None:
    (x, lista) = lista.head
    print(x)
