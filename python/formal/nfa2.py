from functools import reduce
import dfa


def full_zip(states, symbols):
    yield from ((state, symbol) for state in states for symbol in symbols)


class NFA:
    def __init__(self, states, sigma, delta, initial, finals):
        self._states = frozenset(states)
        self._sigma = set(sigma)
        self._delta = delta
        self._initial = initial
        self._finals = frozenset(finals)

    def run(self, w):
        def in_delta(q, x):
            return self._delta[(q, x)] if (q, x) in self._delta else set()

        p = self._initial
        for c in w:
            p = reduce(lambda acc, i: acc | i, map(lambda q: in_delta(q, c), p), set())

        return (p & self._finals) != set()

    def to_dfa(self):
        """Converts this NFA to an equivalent DFA."""
        dfa_states = {tuple()} | {(q,) for q in self._states}

        # Compute all the new states for the DFA
        for state, symbol in full_zip(self._states, self._sigma):
            qs = self._delta.get((state, symbol), tuple())
            dfa_states |= {tuple(qs)}

        # Create a lookup table to rename the new states.
        # Tuples and sets are a bit unwieldy, so simple numbers
        # make it easier, although renaming is not strictly
        # necessary.
        lookup_table = {old: new for new, old in enumerate(dfa_states)}

        # Compute the new transition function, delta.
        # This is done by taking the subset of the powerset
        # of DFA states.
        dfa_delta = dict()
        for state in dfa_states:
            for q, a in full_zip(state, self._sigma):
                dfa_delta[(state, a)] = tuple({qx for qx in self._delta.get((q, a), tuple())})

        # Begin renaming the states for the delta function
        # using a temporary.
        dfa_delta_temp = dict()
        for (inp, sym), out in dfa_delta.items():
            dfa_delta_temp[(lookup_table[inp], sym)] = lookup_table[out]

        # Add entries for the empty state.
        for sym in self._sigma:
            empty = lookup_table[()]
            dfa_delta_temp[(empty, sym)] = empty

        dfa_delta = dfa_delta_temp

        # Compute the initial state.
        dfa_initial = lookup_table[tuple(self._initial)]

        # If any of the NFA final states appear in a DFA state
        # then that DFA state is a final state.
        dfa_final = {lookup_table[qs] for qs in dfa_states for fin in self._finals if fin in qs}

        # Rename the DFA states. Using a set comprehension
        # is faster using the lookup table.
        dfa_states = set(q for q, _ in dfa_delta.keys())

        return dfa.DFA(dfa_states, self._sigma, dfa_delta, dfa_initial, dfa_final)


N0 = NFA(
    {0, 1, 2}, {"0", "1"}, {(0, "0"): {0}, (0, "1"): {0, 1}, (1, "0"): {2}, (1, "1"): {2}}, {0}, {2}
)
