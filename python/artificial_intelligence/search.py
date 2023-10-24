from __future__ import annotations

from dataclasses import dataclass
from queue import Queue
from typing import Any, Callable, Iterable, Optional, TypeVar


class Node:
    def __init__(self, state: Any, parent: Node | None = None, children: list[Node] = []):
        self._state = state
        self._parent = parent
        self._children = children

    @property
    def state(self):
        return self._state

    @property
    def children(self):
        return self._children

    @property
    def parent(self):
        return self._parent

    @parent.setter
    def parent(self, value):
        self._parent = value

    def add_children(self, *children: list[Node]):
        for child in children:
            self.children.append(child)
            child.parent = self
        return self

    def __str__(self) -> str:
        return f"Node({self.state})"

    def __repr__(self) -> str:
        return f"Node({self.state})"


def search(graph: Node, goal_test: Callable[[Any], bool]):
    frontier = Queue()
    frontier.put(graph)
    visited = set()

    node = None
    while not frontier.empty():
        node = frontier.get()

        if node in visited:
            continue

        if goal_test(node.state):
            break
        else:
            for child in node.children:
                frontier.put(child)
            visited.add(node)

    # backtrace
    solution = []
    while node:
        solution.append(node)
        node = node.parent

    return " <- ".join(str(n) for n in solution)


if __name__ == "__main__":
    graph = Node("A").add_children(
        Node("B").add_children(Node("C").add_children(Node("E")), Node("D").add_children(Node("F")))
    )

    solution = search(graph, lambda s: s == "E")

    if solution:
        print(f"Found solution: {solution}")
    else:
        print("No solution found")
