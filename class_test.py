class X:
    x = 5

class Y:
    y = 10

x = X()
x.foo = Y()
x.foo.z = False

should_be_bool = x.foo.z
should_be_int  = x.foo.y
