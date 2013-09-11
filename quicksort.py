#!/usr/bin/env python
import random

def quicksorted(l):
    if len(l) <= 1: # shortcircuit
        return l
    l = l[:]
    pivot_index = random.randint(1, len(l)) - 1
    pivot = l.pop(pivot_index)
    lesser, greater = [], []
    for item in l:
        if item < pivot:
            lesser.append(item)
        else:
            greater.append(item)
    return quicksorted(lesser) + [pivot] + quicksorted(greater)

if __name__ == "__main__":
    import sys
    _item_count = 100000  # default

    if len(sys.argv) < 2:
        item_count = _item_count
    elif len(sys.argv) == 2:
        try:
            item_count = int(sys.argv[1], 10)
        except ValueError:
            item_count = _item_count
    else:
        sys.stderr.write("Too many arguments!\n")
        sys.exit(1)

    sys.stdout.write("Generating random integers...\n")
    items = []
    for i in range(item_count):
        items.append(random.randint(0, 60000))

    sys.stdout.write("Sorting...\n")
    sorted_items = quicksorted(items)

    sys.stdout.write("Done!\n")
    sys.exit(0)
