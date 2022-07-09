"""Mendel's First Law - Rosalind"""
from enum import Enum
import sys

# Gene Enum type
Gene = Enum('Gene', 'dominant heterozygous recessive')

# Make some shortcuts for refering to enum values
Dominant = Gene.dominant
Heterozygous = Gene.heterozygous
Recessive = Gene.recessive

# A list of all posible genes
genes = [Dominant, Heterozygous, Recessive]


def prob(x: int, t: int) -> float:
    """Calculates the probability P(X = x)."""
    return x / t


def pair_prob(fst: int, snd: int, t: int) -> float:
    """Calculate probability of selecting a pair."""
    return prob(fst, t) * prob(snd, t - 1)


def dominance(fst: Gene, snd: Gene) -> float:
    """Calculates the probability of offspring being dominant given the mating Genes."""
    if fst == Gene.recessive and snd == Gene.heterozygous:
        return 0.50
    elif fst == Gene.heterozygous and snd == Gene.recessive:
        return 0.50
    elif fst == Gene.heterozygous and snd == Gene.heterozygous:
        return 0.75
    elif fst == Gene.recessive and snd == Gene.recessive:
        return 0.0
    else:
        return 1.0


def make_combinations(genes):
    """Create a list of pairs of genes."""
    return [(fst, snd) for fst in genes for snd in genes]


def make_population(k, m, n):
    """Create a map relating genes to populations."""
    return {Dominant: k, Heterozygous: m, Recessive: n}


def mate(pair, populations, t):
    """Calculate the probability of offspring from a mating pair being dominant."""
    fst, snd = pair
    if fst == snd:
        return pair_prob(populations[fst], populations[snd] - 1, t) * dominance(fst, snd)
    else:
        return pair_prob(populations[fst], populations[snd], t) * dominance(fst, snd)


def total(pairs, populations, t):
    """Calculate the total probability of all combinations of mating pairs yielding dominant offspring."""
    return round(sum(map(lambda pair: mate(pair, populations, t), pairs)), 5)


if __name__ == '__main__':
    args = sys.argv

    if len(args) != 4:
        raise Exception("Expected exactly 3 arguments")

    k = int(args[1])
    m = int(args[2])
    n = int(args[3])
    t = k + m + n

    result = total(make_combinations(genes), make_population(k, m, n), t)

    print(f"Probability of offspring being dominant:\n{result}")
