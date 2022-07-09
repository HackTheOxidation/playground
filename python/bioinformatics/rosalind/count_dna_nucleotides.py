with open("rosalind_dna.txt") as f:
    content = f.read()

nucleotides = { 'A': 0, 'C': 0, 'G': 0, 'T': 0}
valid_nucleotides = ['A', 'C', 'G', 'T']

for c in content:
    if c in valid_nucleotides:
        nucleotides[c] += 1

print(nucleotides)
