#!/usr/bin/env python
from __future__ import division
import math

def split_factors(n):
    first_fac = math.floor(math.sqrt(n))
    while first_fac >= 1:
        second_fac = n / first_fac
        if isinstance(second_fac, int) or second_fac.is_integer():
            return (first_fac, int(second_fac))
        else:
            first_fac -= 1

def prime_factors(n):
    queue, prime_facs = [n], []
    while queue != []:
        first_fac, second_fac = split_factors(queue.pop(0))
        if first_fac == 1:
            prime_facs.append(second_fac)
        else:
            queue.append(first_fac)
            queue.append(second_fac)
    return prime_facs
