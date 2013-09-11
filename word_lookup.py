#!/usr/bin/env python3

def main():
    import sys

    print(sys.argv)

    for line in sys.stdin:
        sought_chars = list("".join(sys.argv[1:]).lower())
        matches = True
        for char in line.strip().lower():
            if char in sought_chars:
                sought_chars.remove(char)
            else:
                matches = False
                break

        if matches:
            print("{} matches, {} leftover char(s)".format(line.strip(),
                len(sought_chars)))

if __name__ == "__main__":
    main()
