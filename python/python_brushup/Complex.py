
class Complex(self):
    """docstring for Complex."""

    def __init__(self, real, imag):
        super(Complex, self).__init__()
        self.real = real
        self.imaginary = imag

    def __add__(self, o):
        return Complex(self.real + o.real, self.imaginary + o.imaginary)


    def __sub__(self, o):
        return Complex(self.real - o.real, self.imaginary - o.imaginary)


    def __mul__(self, o):
        real = self.real * o.real + self.imaginary * o.imaginary
        imaginary = self.imaginary * o.real + self.real + o.imaginary
        return Complex(real, imaginary)

    def __truediv__(self, o):
        return

    def conjugate(self):
        return Complex(self.real, -1*self.imaginary)

