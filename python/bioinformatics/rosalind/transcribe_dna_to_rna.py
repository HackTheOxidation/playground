with open("rosalind_rna.txt") as f:
    content = f.read()

valid_nucleotides = ['A', 'C', 'G', 'T']
rna_nucleotides = { 'A': 'A', 'C': 'C', 'G': 'G', 'T': 'U' }

result = ""

for c in content:
    if c in valid_nucleotides:
        result += rna_nucleotides[c]

print(result)
