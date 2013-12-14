import sequtils, strutils, parseopt

proc indentedWrap(line: string, maxLineWidth = 80): seq[string] =
    var strippedLine = line.strip(leading=true, trailing=false)

    var indentSize = line.len() - strippedLine.len()
    var wrapWidth = maxLineWidth - indentSize

    var wrappedLines = strippedLine.wordWrap(wrapWidth)

    var prefix = line.substr(0, indentSize - 1)
    return wrappedLines.splitLines().map(proc (s: string): string =
        prefix & s.strip(leading=true, trailing=false))


type TMode = enum
    wrapInput, showUsage

var
    inFiles = @[stdin]
    wrapWidth = 80
    activeMode = wrapInput

for kind, key, val in getopt():
    case kind
    of cmdArgument:
        if inFiles.len() == 1 and inFiles[0] == stdin:
            inFiles = @[]
        var thisFile: TFile
        if open(thisFile, key):
            inFiles = concat(inFiles, @[thisFile])
        else:
            stderr.writeln("WARNING: Failed to open " & key)
    of cmdLongOption, cmdShortOption:
        case key
        of "width", "w":
            try:
                wrapWidth = val.ParseInt()
            except EInvalidValue:
                stderr.writeln("WARNING: " & val & " is not a valid width.")
        of "help", "h":
            activeMode = showUsage
    of cmdEnd:
        assert(false)  # shouldn't be possible

case activeMode
of showUsage:
    for line in @["Usage: wrapnote [OPTION]... [FILE]...",
                  "Output FILE(s), or standard input, to standard output, preserving indentation",
                  "of indented sections.",
                  "",
                  "  -w=WIDTH, --width=WIDTH      wrap to WIDTH characters (default: 80)",
                  "  -h, --help                   display this help and exit"]:
        stdout.writeln(line)
of wrapInput:
    for inFile in inFiles:
        for line in inFile.lines():
            for wrappedLine in line.indentedWrap(wrapWidth):
                stdout.writeln(wrappedLine)
