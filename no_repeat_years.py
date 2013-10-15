#!/usr/bin/env python

def year_ranges(start_year, end_year):
    # first, get the years with no repeated digits
    years = []
    for year in range(start_year, end_year+1):
        if len(str(year)) == len(set(str(year))):
            years.append(year)

    # then produce the ranges
    ranges = []
    for year in years:
        if ranges == []:
            ranges.append((year, year))
        else:
            if (ranges[-1][1] + 1) == year:
                ranges[-1] = (ranges[-1][0], year)
            else:
                ranges.append((year, year))

    return ranges


def main():
    import sys

    # change the range here to get different ranges - the ends are inclusive
    for year_range in year_ranges(2000, 2100):
        sys.stdout.write("—".join([str(year) for year in sorted(list(set(year_range)))]))
        if year_range[1] > year_range[0]:
            sys.stdout.write(" ({} years)".format((year_range[1]-year_range[0])+1))
        sys.stdout.write("\n")


if __name__ == "__main__":
    main()
