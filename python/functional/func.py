from functools import reduce

# Class for demonstration purposes
class Item:
    def __init__(self, cost):
        self.cost = cost


# Mutable, iterative solution
def mutable_total_cart(items):
    total = 0
    for item in items:
        total += item.cost
    return total


# Immutable, explicit recursive solution
def recursive_total_cart(items):
    head, tail = items
    if len(tail) == 0:
        return head.cost
    else:
        return head.cost + recursive_total_cart(tail)


# Immutable, tail call recursive solution
def tail_call_total_cart(items, total=0):
    head, tail = items
    if len(tail) == 0:
        return total + head.cost
    else:
        return tail_call_total_cart(tail, total + head.cost)


# Immutable solution with reduce and lambda for optimized performance
def total_cart(items):
    return reduce(lambda total, head: total + head.cost, items, 0)
