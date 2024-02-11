from dataclasses import dataclass
from typing import Self


@dataclass
class Node:
    neighbours: set['Edge']
    prev: Self
    label: str
    cost: int
    coords: tuple[int, int]


@dataclass
class Edge:
    source: Node
    dest: Node
    weight: int = 1
