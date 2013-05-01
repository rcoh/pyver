empty = []
a = [1,2,False]
should_be_union = a[0]
bad_access = 5[0]

int_list = [1,2,3]
class Foo:
    x = int_list
    y = 5

f = Foo()
should_be_int1 = f.x[3]
should_be_int2 = f.y
should_be_unsub = f.y[2]


