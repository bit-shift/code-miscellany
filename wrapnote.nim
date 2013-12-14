import strutils

proc indentedWrap(line: string, maxLineWidth = 80): seq[string] =
    var strippedLine = line.strip(leading=true, trailing=false)

    var indentSize = len(line) - len(strippedLine)
    var wrapWidth = maxLineWidth - indentSize

    var wrappedLines = wordWrap(strippedLine, maxLineWidth = wrapWidth)

    var prefix = line.substr(0, indentSize - 1)
    return splitLines(wrappedLines).map(proc (s: string): string =
        prefix & s.strip(leading=true, trailing=false))

for line in lines(stdin):
    for wrappedLine in indentedWrap(line):
        writeln(stdout, wrappedLine)
