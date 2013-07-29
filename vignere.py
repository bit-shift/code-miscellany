#!/usr/bin/env python
import math
import string

valid_cleartext_chars = (set(string.ascii_letters) |
                        set(string.digits) |
                        set(string.punctuation) |
                        set(string.whitespace))
valid_key_chars = set(string.ascii_letters)

def vignere_generator(cleartext, key, decode=False):
    assert len(set(cleartext) - set(valid_cleartext_chars)) == 0
    assert len(set(key) - set(valid_key_chars)) == 0

    dumb_repeating_key = (key.upper() * math.ceil(len(cleartext) /
        len(key)))[:len(cleartext)]
    reverse_dumb_key = list(reversed(dumb_repeating_key))

    repeating_key = ""

    for c in cleartext:
        if c in set(string.ascii_letters):
            repeating_key += reverse_dumb_key.pop()
        else:
            repeating_key += " "

    for cleartext_char, key_char in zip(cleartext, repeating_key):
        if key_char == " ":
            yield cleartext_char
        else:
            key_offset = ord(key_char) - ord("A")
            if decode:
                key_offset = 0 - key_offset

            if "A" <= cleartext_char <= "Z":
                cleartext_offset = ord(cleartext_char) - ord("A")
                yield chr(ord("A") + ((26 + cleartext_offset + key_offset) % 26))
            else:
                cleartext_offset = ord(cleartext_char) - ord("a")
                yield chr(ord("a") + ((26 + cleartext_offset + key_offset) % 26))

def vignere_string(cleartext, key, decode=False):
    return "".join(vignere_generator(cleartext, key, decode))
