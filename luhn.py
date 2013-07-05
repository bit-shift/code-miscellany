#!/usr/bin/env python3

def make_check_digit(id_number:str) -> int:
    def double_and_squish(digit:int) -> int:
        doubled_digit = digit * 2
        if doubled_digit > 9:
            return doubled_digit - 9
        else:
            return doubled_digit

    odd_digits = id_number[-1::-2]
    even_digits = id_number[-2::-2]

    wrangled_odd_digits = map(double_and_squish, map(int, odd_digits))
    processed_even_digits = map(int, even_digits)

    summed_digits = sum(wrangled_odd_digits) + sum(processed_even_digits)

    return (summed_digits * 9) % 10

def check(luhned_number:str) -> bool:
    return make_check_digit(luhned_number[:-1]) == int(luhned_number[-1])

def make_valid_number_from_seed(seed:str, length:int = 16) -> str:
    valid_number = seed

    while len(valid_number) < length:
        valid_number += str(make_check_digit(valid_number))

    return valid_number
