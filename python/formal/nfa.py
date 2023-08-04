"""
"""

from itertools import chain, combinations
from functools import reduce
from dfa import DFA


def powerset(iterable: set):
    yield from (set(t) for t in chain.from_iterable(
        combinations(iterable, r) for r in range(len(iterable) + 1)))


class NFA:
    def __init__(self, q, sigma, delta, initial, finals):
        self.q = frozenset(q)
        self.sigma = sigma
        self.delta = delta
        self.initial = initial
        self.finals = finals

    def run(self, w):
        in_delta = lambda q, x: self.delta[(q, x)] if (q, x) in self.delta else set()

        p = self.initial
        for c in w:
            p = reduce(
                lambda acc, i: acc | i, map(lambda q: in_delta(q, c), p), set()
            )

        return (p & self.finals) != set()

    def to_dfa(self):
        states = { frozenset(q) for q in powerset(self.q) if q in self.delta.values() }
        delta_dfa = dict()
        for R in states:
            for r in R:
                delta_dfa |= {(R, a): { q for q in self.q if q in self.delta.get((r, a), set())} 
                              for a in self.sigma}

        return DFA(states, self.sigma, delta_dfa, self.initial, self.finals)

    @staticmethod
    def from_dfa(dfa: DFA):
        delta_nfa = { qx: frozenset(qq) for qx, qq in dfa.delta.items() }
        return NFA(dfa.q, dfa.sigma, delta_nfa, frozenset(dfa.q0), dfa.f)


N0 = NFA(
    {0, 1, 2},
    {"0", "1"},
    {(0, "0"): {0}, (0, "1"): {0, 1}, (1, "0"): {2}, (1, "1"): {2}},
    {0},
    {2})

if __name__ == "__main__":

    print(f"{N0.run('10')}")
    print(f"{N0.run('100')}")
    print(f"{N0.run('010')}")
    print(f"{N0.run('1')}")
