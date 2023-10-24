class T1:
    pass


class T2(T1):
    pass


def f1(p: T1) -> None:
    pass


o2 = T2()

f1(o2)


def f2(p: T2) -> None:
    pass


o1 = T1()

f2(o1)
