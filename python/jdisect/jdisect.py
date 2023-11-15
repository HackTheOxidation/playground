"""
Utility for disassembling binary Java .class files
  into human-readable assembly-like bytecode files.
"""

import sys
import logging

from opcodes import opcode_table

logger = logging.getLogger(__name__)


class JavaDisassembler:
    def __init__(self, src_file, out_file='out.jasm'):
        self._src_file = src_file
        self._out_file = out_file

    def disassemble(self):
        with open(self._src_file, 'rb') as src:
            hex_content = [hex(b) for b in src.read()]

        with open(self._out_file, 'w') as out: 
            for i, b in enumerate(hex_content):
                if (mnemonic := opcode_table.get(int(b, 16))):
                    out.write(f'{i}: {mnemonic}\n')
                else:
                    logger.warning(f'Unknown opcode: {b}')
                

if __name__ == '__main__':
    match sys.argv:
        case [_, src_file, *args]:
            disassembler = JavaDisassembler(src_file, *args)
            disassembler.disassemble()
        case _:
            print('jdisect - Invalid number of arguments: expect 1 (source file).')
            sys.exit(1)
