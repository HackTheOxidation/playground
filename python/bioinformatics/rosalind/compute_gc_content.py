"""
Module for reading FASTA files and computing GC content in strands.
"""

class FastaFormatError(BaseException):
    pass


class FastaEntry:

    def __init__(self, label=None, content=None):
        self.label = label
        self.content = content

    def compute_gc_content(self):
        if self.label is not None and self.content is not None:
            gc_content = 0
            for nucleotide in self.content:
                if nucleotide == 'C' or nucleotide == 'G':
                    gc_content += 1

            return (gc_content / len(self.content)) * 100.0
        else:
            raise FastaFormatError("Formatting Error: label or content is not defined.")

    def __str__(self):
        return f"{self.label}"


class FastaFile:

    def __init__(self, lines):
        self.entries = self.generate_entries(lines)

    def generate_entries(self, lines) -> list[FastaEntry]:
        entries: list[FastaEntry] = []
        current_entry: FastaEntry = None 
        content = ""

        for line in lines:
            if line.startswith('>'):
                if current_entry is not None:
                    current_entry.content = content.strip()
                    entries.append(current_entry)
                    content = ""

                current_entry = FastaEntry(label=line.strip().replace('>', ''))
            else:
                content += line.strip()

        current_entry.content = content.strip()
        entries.append(current_entry)

        return entries

    def compute_gc_content(self):
        highest_gc: tuple(FastaEntry, float) = None
        for entry in self.entries:
            gc_content = entry.compute_gc_content()
            if highest_gc is not None:
                if gc_content > highest_gc[1]:
                    highest_gc = (entry, gc_content)
            else:
                highest_gc = (entry, gc_content)

        return highest_gc

if __name__ == '__main__':
    with open("rosalind_gc.txt") as f:
        content = f.readlines()

    fasta_file = FastaFile(lines=content)
    entry, gc_content = fasta_file.compute_gc_content()
    print(f"{entry}\n{gc_content}")
