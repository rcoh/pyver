class X:
    p = 1
    r = 1
    s = 5

class Y:
    q = False
    r = False
    s = 5


a = [X, Y]

class_obj = a[0]
inst = class_obj()

inst.t = 5
missing_ident_and_int = inst.p
missing_ident_and_bool = inst.q
bool_or_int = inst.r
just_int = inst.s
just_int2 = inst.t


