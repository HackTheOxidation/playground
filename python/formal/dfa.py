"""
"""
from functools import reduce


class DFA:
    def __init__(self, q, sigma, delta, q0, f):
        self.q = frozenset(q)
        self.sigma = sigma
        self.delta = delta
        self.q0 = q0
        self.f = f

    def run(self, w):
        return reduce(lambda q, c: self.delta[(q, c)], w, self.q0) in self.f

    def minimize(self):
        pass
