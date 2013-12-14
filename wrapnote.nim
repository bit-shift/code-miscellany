import strutils


for line in lines(stdin):
    if startsWith(line, "   "):
        for wrappedLine in splitLines(wordWrap(unindent(line, true), maxLineWidth = 77)):
            writeln(stdout, "   " & wrappedLine)
    else:
        writeln(stdout, line)
