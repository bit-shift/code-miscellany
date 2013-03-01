#!/usr/bin/env python
from prime_factors import prime_factors

def sum_digits(n):
    return sum(int(d) for d in str(n))

def is_smith_number(n):
    sum_factor_digits = sum(sum_digits(f) for f in prime_factors(n))
    return sum_factor_digits == sum_digits(n)
