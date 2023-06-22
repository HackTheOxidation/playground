"""
"""

from functools import reduce
from dfa import DFA


class NFA:
    def __init__(self, q, sigma, delta, initial, finals):
        self.q = q
        self.sigma = sigma
        self.delta = delta
        self.initial = initial
        self.finals = finals

    def run(self, w):
        in_delta = lambda q, x: self.delta[(q, x)] if (q, x) in self.delta else set({})

        p = self.initial
        for c in w:
            p = reduce(
                lambda acc, i: acc | i, map(lambda q: in_delta(q, c), p), set({})
            )

        return (p & self.finals) != set({})

    def to_dfa(self):
        pass

    @staticmethod
    def from_dfa(dfa):
        delta_nfa = { qx: {qq} for qx, qq in self.delta.items() }
        return NFA(dfa.q, dfa.sigma, delta_nfa, {dfa.q0}, dfa.f)


if __name__ == "__main__":
    N0 = NFA(
        {0, 1, 2},
        {"0", "1"},
        {(0, "0"): {0}, (0, "1"): {0, 1}, (1, "0"): {2}, (1, "1"): {2}},
        {0},
        {2},
    )

    print(f"{N0.run('10')}")
    print(f"{N0.run('100')}")
    print(f"{N0.run('010')}")
    print(f"{N0.run('1')}")
