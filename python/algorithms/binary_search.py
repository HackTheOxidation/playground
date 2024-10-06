
def binary_search(collection, key):
    def _dispatch():
        idx = len(collection) // 2
        k = collection[idx]
        if k == key:
            return idx
        elif k < key:
            return binary_search(collection[idx:], key)
        else:
            return binary_search(collection[:idx], key)

    match collection:
        case []:
            return None
        case [k] if k != key:
            return None
        case [k] if k == key:
            return 0
        case _:
            return _dispatch()


if __name__ == '__main__':
    print(f"Index of '3' in '[1, 2, 3, 4, 5] is {binary_search([1, 2, 3, 4, 5], 3)}'")
    print(f"Index of '3' in '[0, 1, 2, 3, 4, 5] is {binary_search([0, 1, 2, 3, 4, 5], 3)}'")
    print(f"Index of '42' in '[1, 2, 3, 4, 5] is {binary_search([1, 2, 3, 4, 5], 42)}'")
