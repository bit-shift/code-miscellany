#!/usr/bin/env python
import re

# actions, effect on state:
#   l is current length, o is current octave
# 1—7       insert lCo through lBo
# hold 1—7  insert l.Co through l.Bo
# 8         double l
# 9         halve l
# *         o = (((o - 1) + 1) % 3) + 1
# 0         insert l-
# #         toggle sharpness of last note

note_regex = re.compile(r"(1|2|4|8|16|32)(\.?)(\#?)([A-Ga-g\-])([1-3]?)")

def next_keys_and_state(next_length, next_octave, sharpness, extra_long, note,
        current_length=4, current_octave=1):
    keys = []

    keys.append(str("-CDEFGAB".index(note)))

    if extra_long:
        keys[-1] = "hold " + keys[-1]

    if sharpness:
        keys.append("#")

    temp_length = current_length
    if next_length > temp_length:
        while temp_length < next_length:
            keys.append("8")
            temp_length *= 2
    elif next_length < temp_length:
        while temp_length > next_length:
            keys.append("9")
            temp_length //= 2

    if next_octave == 0:
        next_octave = current_octave
        next_length = current_length

    while current_octave != next_octave:
        keys.append("*")
        current_octave = (current_octave % 3) + 1

    return (keys, next_length, next_octave)

def note_from_string(note_string):
    raw_note = note_regex.match(note_string)
    if raw_note is None:
        return None
    else:
        note_parts = raw_note.groups()
        return (int(note_parts[0]),
                int(note_parts[4]) if note_parts[4] != "" else 0,
                note_parts[2] == "#",
                note_parts[1] == ".",
                note_parts[3].upper())

def main():
    import sys

    # state
    current_length = 4
    current_octave = 1

    for line in sys.stdin:
        for possible_note in line.split():
            note = note_from_string(possible_note)
            if note is None:
                sys.stdout.write(("Invalid note '{}' in input. " +
                        "Stopping.\n").format(possible_note))
            else:
                keys, current_length, current_octave = next_keys_and_state(
                        *note,
                        current_length=current_length,
                        current_octave=current_octave
                        )
                sys.stdout.write("{}\n".format("  ".join(keys)))
                #for key in keys:
                    #sys.stdout.write("{}\n".format(key))

if __name__ == "__main__":
    main()
