with open("rosalind_revc.txt") as f:
    content = f.read()


valid_nucleotides = ['A', 'C', 'G', 'T']
complements = { 'A': 'T', 'C': 'G', 'G': 'C', 'T': 'A' }
result = ""

for c in content:
    if c in valid_nucleotides:
        result += complements[c]

reverse_complement = result[::-1]

print(reverse_complement)
