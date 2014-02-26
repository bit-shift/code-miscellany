#!/usr/bin/env python

def expand_ranges(s):
    expanded_s = ""
    while s != "":
        if len(s) > 2 and s[1] == "-":
            start, _, end, *rest = s
            start, end = map(ord, [start, end])
            s = "".join(rest)
            if start == end:
                expanded_s += chr(start)
            else:
                if start < end:
                    seq = range(start, end+1)
                else:
                    seq = range(start, end-1, -1)
                for n in seq:
                    expanded_s += chr(n)
        else:
            expanded_s += s[0]
            s = s[1:]

    return expanded_s


def main(self_name, args):
    import sys

    if len(args) != 2:
        print("Wrong number of args - invocation is {} <in-set> <out-set>".format(self_name))
        return 1

    in_set, out_set = map(expand_ranges, args)
    out_set += out_set[-1:] * (len(in_set) - len(out_set))  # extend out set

    trans = str.maketrans(in_set, out_set)

    for line in sys.stdin:
        sys.stdout.write(line.translate(trans))


if __name__ == "__main__":
    import sys
    sys.exit(main(sys.argv[0], sys.argv[1:]) or 0)
