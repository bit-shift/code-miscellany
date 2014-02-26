class Memoized(object):
    def __init__(self, fn):
        self.fn = fn
        self.memo = {}

    def __call__(self, *args):
        if not args in self.memo:
            self.memo[args] = self.fn(*args)
        return self.memo[args]

@Memoized
def get_hyperop(n):
    if n < 0:
        raise Exception("Hyperoperations are undefined for n < 0")
    if not isinstance(n, int):
        raise Exception("Hyperoperations are undefined for non-integral numbers.")

    def hyperop(a, b):
        if n == 0:
            return b + 1
        elif b == 0:
            if n == 1:
                return a
            elif n == 2:
                return 0
            else:
                return 1
        else:
            return get_hyperop(n - 1)(a, hyperop(a, b - 1))

    return hyperop
