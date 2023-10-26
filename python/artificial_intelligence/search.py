from __future__ import annotations

from dataclasses import dataclass, field
from queue import Queue
from typing import Any, Callable


@dataclass(unsafe_hash=True)
class Node:
    state: Any
    parent: Node | None = None
    children: frozenset[Node] = field(default_factory=frozenset)

    def set_children(self, *children):
        self.children = frozenset(children)
        for child in self.children:
            child.parent = self
        return self

    def __str__(self) -> str:
        return f"Node({self.state})"

    def __repr__(self) -> str:
        return f"Node({self.state})"


def search(graph: Node, goal_test: Callable[[Any], bool]):
    frontier: Queue[Node] = Queue()
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
    graph = Node("A").set_children(
        Node("B").set_children(Node("C").set_children(Node("E")), Node("D").set_children(Node("F")))
    )

    solution = search(graph, lambda s: s == "E")

    if solution:
        print(f"Found solution: {solution}")
    else:
        print("No solution found")
