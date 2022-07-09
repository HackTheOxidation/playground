
## Importing stuff from modules
import sys
import json
import mymodule.echo as ec
from myothermodule.cubical import *

ec.printecho("Hello")
print(cube(3))
print(linear_model(3, 5, 1))

## File handling
with open('some_dumb_file.txt') as f:
    content = f.read()


f = open('some_dumb_file.txt', 'r+')
for line in f:
    print(line, end='')

f.write('This is another dumb line.\n')
f.close()

f = open('file_for_json.txt', 'r+')

x = [1, "simple", "list"]

json.dump(x,f)

f.close()

print(x)

## Some exception handling
try:
    f = open('some_file.txt', 'r+')
except:
    print("There ain't no file here")
finally:
    print("That's it, folks")


## Making a class

class MyClass(self):
    """docstring for MyClass."""

    ## Constructor
    def __init__(self, arg):
        super(MyClass, self).__init__()
        self.arg = arg

    def function_for_MyClass():
        print("This is a great class")

    def ret_hello():
        return "Hello there!"


obj = MyClass()
obj.function_for_MyClass()
print(obj.ret_hello())

# Inheritance
class MyOtherClass(MyClass):
    """docstring for MyOtherClass."""

    def __init__(self, arg):
        super(MyOtherClass, self).__init__()

        self.arg = arg

    def function_for_MyOtherClass():
        print("This is also a great class")


obj2 = MyOtherClass()
obj.function_for_MyOtherClass()
print(obj2.ret_hello())


# Struct look-alike
class Employee:
    pass


john = Employee()

john.name = "John Doe"
john.dept "computer lab"
john.salary = 1000

# Iterators

for element in [1, 2, 3]:
    print(element)
for element in (1, 2, 3):
    print(element)
for key in {'one':1, 'two':2}:
    print(key)
for char in "123":
    print(char)

s = 'abc'
it = iter(s)
print(next(it))
print(next(it))

# Making an Iterator class
class Reverse:
    """docstring for Reverse."""

    # Constructor
    def __init__(self, data):
        self.data = data
        self.index = len(data)

    # Overrides the iterator function
    def __iter__(self):
        return self

    # Overrides the next-iterator function
    def __next__(self):
        if self.index == 0:
            raise StopIteration
        self.index = self.index - 1
        return self.data[self.index]


rev = Reverse('spam')
iter(rev)
for char in rev:
    print(char)
