"""
"""

class DFA:
    def __init__(self, q, sigma, delta, q0, f):
        self.q = q
        self.sigma = sigma
        self.delta = delta
        self.q0 = q0
        self.f = f

    def run(self, w):
        q = self.q0
        for c in w:
            q = self.delta[(q, c)]

        return q in self.f

    def minimize(self):
        pass
