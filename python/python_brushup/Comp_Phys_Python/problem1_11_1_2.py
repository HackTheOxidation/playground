from pylab import *

y1 = float(input('Enter the initial height: '))
v1 = float(input('Enter the initial velocity: '))

t = linspace(0, 5.0, 50)

yt = y1 + v1*t - 0.5*9.8*t**2

print(yt)
