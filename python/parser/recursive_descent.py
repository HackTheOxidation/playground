""""""
from dataclasses import dataclass


class InvalidSyntaxError(BaseException):
    pass


class Node:
    pass


@dataclass
class Pair(Node):
    left: Node
    right: Node

    def __str__(self):
        return f"Pair({self.left} {self.right})"


@dataclass
class Char(Node):
    value: str

    def __str__(self):
        return f"Char({self.value})"


def parse_pair(source: str):
    if not source or source[0] != '(':
        raise InvalidSyntaxError("Expected: '('")

    left, remaining = parse(source[1:])

    if not remaining or remaining[0] != ' ':
        raise InvalidSyntaxError("Expected: ' '")

    right, remaining = parse(remaining[1:])

    if not remaining or remaining[0] != ')':
        raise InvalidSyntaxError("Expected: ')'")

    return Pair(left=left, right=right), remaining[1:]


def parse_char(source: str):
    return Char(source[0]), source[1:]


def parse(source: str):
    if not source or source[0] == ' ':
        raise InvalidSyntaxError()

    if source[0] == '(':
        return parse_pair(source)
    else:
        return parse_char(source)


if __name__ == '__main__':
    print(parse('(a (b c))')[0])
