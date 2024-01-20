import sys


def maximum_profit(profits):
    m = None
    for k, v in profits.items():
        if not m:
            m = (k, v)
            continue

        if m and v > m[1]:
            m = (k, v)

    return m[0] if m else None
            

def chocolate_profit(x, s, b, *args):
    if x < 0 or x >= len(b) - 1 or len(s) != len(b):
        return None

    def profit(i, j):
        return b[j] - s[i] - (j - i) * 100

    def recurse(i, j):
        if j == len(b) - 1:
            return { (i, j): profit(i, j) }
        elif i == x:
            return { (i, j): profit(i, j) } | recurse(i, j + 1)
        else:
            return recurse(i + 1, j) | { (i, j): profit(i, j) } | recurse(i, j + 1)

    return recurse(0, x + 1)


if __name__ == '__main__':
    params = (sys.argv[0:])[1:]

    max_profit = maximum_profit(chocolate_profit(*(eval(p) for p in params)))

    print(f"The maximum_profit is {max_profit}")

