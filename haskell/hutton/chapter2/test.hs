double x = x + x

quadruple x = double (double x)

-- Factorial of a positive integer:
factorial n = product [1..n]

-- Average of a list of integers (with infix function call):
average ns = sum ns `div` length ns
