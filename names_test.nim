import strutils

var call_count = 0

proc ordinal(n: int): string =
    let n_str = $n
    if n_str.endsWith("1") and not n_str.endsWith("11"):
        return n_str & "st"
    elif n_str.endsWith("2") and not n_str.endsWith("12"):
        return n_str & "nd"
    elif n_str.endsWith("3") and not n_str.endsWith("13"):
        return n_str & "rd"
    else:
        return n_str & "th"

proc my_proc =
    var ordinal: string

    call_count += 1

    echo("Called for the " & ordinal(call_count) & " time!")

for n in 1..6:
    my_proc()
    myproc()
    myProc()
    MyProc()
    my_Proc()
