with open("rosalind_hamm.txt") as f:
    content = f.readlines()

first = content[0]
second = content[1]

if len(first) != len(second):
    raise ValueError("Strand lengths are different!")

hamming_distance = 0

for i in range(len(first)):
    if first[i] != second[i]:
        hamming_distance += 1

print(hamming_distance)
