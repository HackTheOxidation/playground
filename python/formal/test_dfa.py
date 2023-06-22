from dfa import DFA
from unittest import TestCase


class TestDFA(TestCase):
    def setUp(self):
        super().setUp()

        self.d0 = DFA(
            {0, 1, 2},
            {"a", "b"},
            {
                (0, "a"): 0,
                (0, "b"): 1,
                (1, "a"): 2,
                (1, "b"): 1,
                (2, "a"): 2,
                (2, "b"): 2,
            },
            0,
            {0, 1},
        )

        self.d1 = DFA(
            {0, 1, 2, 3},
            {"a", "b"},
            {
                (0, "a"): 2,
                (0, "b"): 1,
                (1, "a"): 0,
                (1, "b"): 3,
                (2, "a"): 0,
                (2, "b"): 3,
                (3, "a"): 1,
                (3, "b"): 2,
            },
            0,
            {0, 3},
        )

        # example from minimisation.pdf
        # test case : D2.minimize()
        self.d2 = DFA(
            {0, 1, 2, 3, 4, 5},
            {"a", "b"},
            {
                (0, "a"): 1,
                (0, "b"): 4,
                (1, "a"): 2,
                (1, "b"): 3,
                (2, "a"): 2,
                (2, "b"): 2,
                (3, "a"): 2,
                (3, "b"): 3,
                (4, "a"): 5,
                (4, "b"): 4,
                (5, "a"): 5,
                (5, "b"): 4,
            },
            0,
            {2, 3},
        )

    def test_dfa_d0_empty(self):
        self.assertTrue(self.d0.run(""))

    def test_dfa_d0_aa(self):
        self.assertTrue(self.d0.run("aa"))

    def test_dfa_d0_aabbb(self):
        self.assertTrue(self.d0.run("aabbb"))

    def test_dfa_d0_ba(self):
        self.assertFalse(self.d0.run("ba"))

    def test_dfa_d1_aa(self):
        self.assertTrue(self.d1.run("aa"))

    def test_dfa_d1_a(self):
        self.assertFalse(self.d1.run("a"))
