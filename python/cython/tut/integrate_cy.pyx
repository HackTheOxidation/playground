import cython

@cython.cfunc
@cython.exceptval(-2, check=True)
def f(x: cython.double) -> cython.double:
    return x ** 2 - x


def integrate_f(a: cython.double, b: cython.double, N: cython.int) -> cython.double:
    i: cython.int
    s: cython.double = 0
    dx: cython.double = (b - a) / N
    for i in range(N):
        s += f(a + i * dx)
    return s * dx
