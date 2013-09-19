;;; PRESET CHARS FOR QUICK OUTPUT (FIRST 43 CELLS) ;;;

>++[<+++++>-][-]<            ; set '\n' using a counter; zero the counter

[>>+<<-]>>[<<+>+>-]<         ; clone '\n'
++>+++++[<++++>-][-]<        ; increase to ' ' with a counter; zero the counter

[>>+<<-]>>[<<+>+>-]<         ; clone ' '
+++                          ; increase to '#'

[>>+<<-]>>[<<+>+>-]<         ; clone '#'
+++++                        ; increase to '('

[>>+<<-]>>[<<+>+>-]<         ; clone '('
+                            ; increase to ')'

[>>+<<-]>>[<<+>+>-]<         ; clone ')'
+                            ; increase to '*'

[>>+<<-]>>[<<+>+>-]<         ; clone '*'
+                            ; increase to PLUS

[>>+<<-]>>[<<+>+>-]<         ; clone PLUS
+                            ; increase to COMMA

[>>+<<-]>>[<<+>+>-]<         ; clone COMMA
+                            ; increase to MINUS

[>>+<<-]>>[<<+>+>-]<         ; clone MINUS
+                            ; increase to POINT

[>>+<<-]>>[<<+>+>-]<         ; clone POINT
++                           ; increase to '0'

[>>+<<-]>>[<<+>+>-]<         ; clone '0'
+++                          ; increase to '3'

[>>+<<-]>>[<<+>+>-]<         ; clone '3'
+++++++                      ; increase to ':'

[>>+<<-]>>[<<+>+>-]<         ; clone ':'
+                            ; increase to ';'

[>>+<<-]>>[<<+>+>-]<         ; clone ';'
+                            ; increase to LESS THAN

[>>+<<-]>>[<<+>+>-]<         ; clone LESS THAN
+                            ; increase to '='

[>>+<<-]>>[<<+>+>-]<         ; clone '='
+                            ; increase to MORE THAN

[>>+<<-]>>[<<+>+>-]<         ; clone MORE THAN
+                            ; increase to '?'

[>>+<<-]>>[<<+>+>-]<         ; clone '?'
++++++                       ; increase to 'E'

[>>+<<-]>>[<<+>+>-]<         ; clone 'E'
+                            ; increase to 'F'

[>>+<<-]>>[<<+>+>-]<         ; clone 'F'
+++++++++                    ; increase to 'O'

[>>+<<-]>>[<<+>+>-]<         ; clone 'O'
++++++++++++                 ; increase to LEFT BRACKET

[>>+<<-]>>[<<+>+>-]<         ; clone LEFT BRACKET
++                           ; increase to RIGHT BRACKET

[>>+<<-]>>[<<+>+>-]<         ; clone RIGHT BRACKET
++++                         ; increase to 'a'

[>>+<<-]>>[<<+>+>-]<         ; clone 'a'
++                           ; increase to 'c'

[>>+<<-]>>[<<+>+>-]<         ; clone 'c'
+                            ; increase to 'd'

[>>+<<-]>>[<<+>+>-]<         ; clone 'd'
+                            ; increase to 'e'

[>>+<<-]>>[<<+>+>-]<         ; clone 'e'
++                           ; increase to 'g'

[>>+<<-]>>[<<+>+>-]<         ; clone 'g'
+                            ; increase to 'h'

[>>+<<-]>>[<<+>+>-]<         ; clone 'h'
+                            ; increase to 'i'

[>>+<<-]>>[<<+>+>-]<         ; clone 'i'
+++                          ; increase to 'l'

[>>+<<-]>>[<<+>+>-]<         ; clone 'l'
+                            ; increase to 'm'

[>>+<<-]>>[<<+>+>-]<         ; clone 'm'
+                            ; increase to 'n'

[>>+<<-]>>[<<+>+>-]<         ; clone 'n'
+                            ; increase to 'o'

[>>+<<-]>>[<<+>+>-]<         ; clone 'o'
+                            ; increase to 'p'

[>>+<<-]>>[<<+>+>-]<         ; clone 'p'
++                           ; increase to 'r'

[>>+<<-]>>[<<+>+>-]<         ; clone 'r'
+                            ; increase to 's'

[>>+<<-]>>[<<+>+>-]<         ; clone 's'
+                            ; increase to 't'

[>>+<<-]>>[<<+>+>-]<         ; clone 't'
+                            ; increase to 'u'

[>>+<<-]>>[<<+>+>-]<         ; clone 'u'
+                            ; increase to 'v'

[>>+<<-]>>[<<+>+>-]<         ; clone 'v'
+                            ; increase to 'w'

[>>+<<-]>>[<<+>+>-]<         ; clone 'w'
++++                         ; increase to '{'

[>>+<<-]>>[<<+>+>-]<         ; clone '{'
++                           ; increase to '}'


;;; HEADER ;;;

;; INCLUDE STDIO
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>>>>>>>>>>>>.
>>>.
<<<<<<<<.
>>>>>>.
>>>>>>>>.
<<<<<<<<<<<<<.
>.
<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>.
>>>>>>>>>>>>>>>>>>>>>>.
>.
<<<<<<<<<<<<.
>>>>.
>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>>>>.
<<<<<<<<<<<<.
<<<<<<<<<<<<<<<<.

;; INCLUDE STRING
>>.
>>>>>>>>>>>>>>>>>>>>>>>>>>>.
>>>.
<<<<<<<<.
>>>>>>.
>>>>>>>>.
<<<<<<<<<<<<<.
>.
<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>.
>>>>>>>>>>>>>>>>>>>>>>.
>.
<<.
<<<<<<.
>>>.
<<<<<.
<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>>>>.
<<<<<<<<<<<<.
<<<<<<<<<<<<<<<<.

;; BLANK LINE
.

;; INT MAIN
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
>>>.
>>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
<<<<<<<<.
>>>>>>.
>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>>>>>>>>>>>.
>>>.
>>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>>>>>>>.
>>>>>>>>>>>>.
<<<<<<<<.
<<<.
<<<<<<<<<<<<<<<<<.
<<<<<<.
>>>>>>>>>>>>>>>>>>>>>>>.
>>>>.
<<<<<.
>>>>>>>>>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>.
>>>>>>>>>>>>>>>>>>.
>>>>>>>>>>>>.
<<<<<<<<.
>>>>>>>>>>>>.
<<<<<<<<<<<<<<<<<<.
>.
<<<<<<<<<<<<<<<<<<.
<<<.
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.

;; CHAR DATA
>>>>>>>>>>>>>>>>>>>>>>>>.
>>>>.
<<<<<.
>>>>>>>>>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>>>>>>>>>.
<<.
>>>>>>>>>>>>>>.
<<<<<<<<<<<<<<.
<<.
<<<<<<<<<<.
<.
.
.
.
>>>>>>>>>>>>.
<<<<<<<<<.
<<<<<<<<<<<<<.

;; CHAR PTR
>>>>>>>>>>>>>>>>>>>>>>>>.
>>>>.
<<<<<.
>>>>>>>>>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>.
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
>>>.
<<.
<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>.
<<<<<.
>>>>>.
>>>>>.
<<<<<<<<<<.
>>>>>>>>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>>>>>>>.
<<.
>>>>>>>>>>>>>>.
<<<<<<<<<<<<<<.
<<<<<<<<<<<<<<<<.
<<<<<<.
>>>>>>>>>.
<<<.
<<<<<<.
>>>>>>>>>>.
<.
.
.
.
<<<<<<.
>>>>>>>>>.
<<<<<<<<<<<<<.

;; INT IN
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
>>>.
>>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
>>>.
<<<<<<<<<<<<<<<<<.
<<<<<.
>>>.
<<<<<<<<<<<<<.

;; BLANK LINE
.


;;; READ/SWITCH/PRINT LOOP ;;;

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>,[ ; move to cell 44 and read input ; begin loop

    ;; CHECK FOR PLUS
    [>>+<<-]>>[<<+>+>-]<         ; clone input into cell 45 ; ends at cell 40
    >[-]+<                       ; set cell 46 to 1
    ->>[-]++++++[<<------->>-]<< ; decrease cell 45 by 43
    [                            ; loop as an if ; only runs if cell 45 is 0
        >[-]                     ; this *was not* a PLUS ; unset cell 46
        <[-]                     ; and back to cell 45 ; drain so loop exits
    ]
    >[                           ; another if loop; only runs if cell 46 nonzero
        ;; PLUS FOUND ; PRINT CODE
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
        .
        <.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
        >>>.
        <<.
        <<<<<<<<<<<<<<<<<<<<<<.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        [-]
    ]<<

    ;; CHECK FOR COMMA
    [>>+<<-]>>[<<+>+>-]<         ; clone input into cell 45 ; ends at cell 45
    >[-]+<                       ; set cell 46 to 1
    -->>[-]++++++[<<------->>-]<<; decrease cell 45 by 44
    [                            ; loop as an if ; only runs if cell 45 is 0
        >[-]                     ; this *was not* a COMMA ; unset cell 46
        <[-]                     ; and back to cell 45 ; drain so loop exits
    ]
    >[                           ; another if loop; only runs if cell 46 nonzero
        ;; COMMA FOUND ; PRINT CODE
        <<<<<<<<<<<<<<<<.
        >>>.
        <<<<<<<<<<<<<<<<<.
        >>>>>>>>>>>>.
        <.
        >>>>>>>>>>>.
        <<<<<<<<<<<<<.
        >>>>.
        <<<<<.
        >>>>>>>>>>>>.
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
        >.
        >>>>>>>>>.
        <<<<<<<<.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
        >>>.
        <<.
        <<<<<<<<<<<<<<<<<<<<.
        <<<<<<<<<<<<.
        .
        >>>>>>>>>>>>>>>>>>>>>>>>>>.
        >>>.
        <<<<<<<<<<<<<<<<<.
        .
        >>>.
        >>.
        <.
        <<<<<<<<<<<<<<<.
        >>>>>>>>>>>>>.
        <<<<<<<.
        >>.
        >>>>>>>>>>>>>>>>>.
        >>>.
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<.
        >>>>>>>>>.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        [-]
    ]<<

    ;; CHECK FOR MINUS
    [>>+<<-]>>[<<+>+>-]<         ; clone input into cell 45 ; ends at cell 45
    >[-]+<                       ; set cell 46 to 1
    >>[-]+++++[<<--------->>-]<< ; decrease cell 45 by 45
    [                            ; loop as an if ; only runs if cell 45 is 0
        >[-]                     ; this *was not* a MINUS ; unset cell 46
        <[-]                     ; and back to cell 45 ; drain so loop exits
    ]
    >[                           ; another if loop; only runs if cell 46 nonzero
        ;; MINUS FOUND ; PRINT CODE
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
        .
        <<<.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
        >>>.
        <<.
        <<<<<<<<<<<<<<<<<<<<<<.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        [-]
    ]<<

    ;; CHECK FOR POINT
    [>>+<<-]>>[<<+>+>-]<         ; clone input into cell 45 ; ends at cell 45
    >[-]+<                       ; set cell 46 to 1
    ->>[-]+++++[<<--------->>-]<<; decrease cell 45 by 46
    [                            ; loop as an if ; only runs if cell 45 is 0
        >[-]                     ; this *was not* a POINT ; unset cell 46
        <[-]                     ; and back to cell 45 ; drain so loop exits
    ]
    >[                           ; another if loop; only runs if cell 46 nonzero
        ;; POINT FOUND ; PRINT CODE
        <<<<<<<<<<<.
        >>>>.
        <.
        <<<<<<<<<<<<<.
        >>>>.
        <<<<<.
        >>>>>>>>>>>>.
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
        >>.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
        >>>.
        <<.
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
        >>>>>>>>>.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        [-]
    ]<<

    ;; CHECK FOR LESS THAN
    [>>+<<-]>>[<<+>+>-]<         ; clone input into cell 45 ; ends at cell 45
    >[-]+<                       ; set cell 46 to 1
    ++++>>[-]++++++++[<<-------->>-]<<  ; decrease cell 45 by 60
    [                            ; loop as an if ; only runs if cell 45 is 0
        >[-]                     ; this *was not* a LESS THAN ; unset cell 46
        <[-]                     ; and back to cell 45 ; drain so loop exits
    ]
    >[                           ; another if loop; only runs if cell 46 nonzero
        ;; LESS THAN FOUND ; PRINT CODE
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
        .
        >>>>>>>>>>>>>>>>>>>>>>>>>>.
        >>>.
        <<.
        <<<<<<<<<<<<<<<<<<<<<<.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        [-]
    ]<<

    ;; CHECK FOR MORE THAN
    [>>+<<-]>>[<<+>+>-]<         ; clone input into cell 45 ; ends at cell 45
    >[-]+<                       ; set cell 46 to 1
    ++>>[-]++++++++[<<-------->>-]<<  ; decrease cell 45 by 62
    [                            ; loop as an if ; only runs if cell 45 is 0
        >[-]                     ; this *was not* a MORE THAN ; unset cell 46
        <[-]                     ; and back to cell 45 ; drain so loop exits
    ]
    >[                           ; another if loop; only runs if cell 46 nonzero
        ;; MORE THAN FOUND ; PRINT CODE
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
        .
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>.
        >>>.
        <<.
        <<<<<<<<<<<<<<<<<<<<<<.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        [-]
    ]<<

    ;; CHECK FOR LEFT BRACKET
    [>>+<<-]>>[<<+>+>-]<         ; clone input into cell 45 ; ends at cell 45
    >[-]+<                       ; set cell 46 to 1
    ->>[-]+++++++++[<<---------->>-]<<  ; decrease cell 45 by 91
    [                            ; loop as an if ; only runs if cell 45 is 0
        >[-]                     ; this *was not* a LEFT BRACKET ; unset cell 46
        <[-]                     ; and back to cell 45 ; drain so loop exits
    ]
    >[                           ; another if loop; only runs if cell 46 nonzero
        ;; LEFT BRACKET FOUND ; PRINT CODE
        <<<<<.
        <<<<<<<<<<<<.
        >.
        >.
        <<<<.
        <<<<<<<<<<<<<<<<<<<<<<<.
        >>.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
        >>>.
        <<.
        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
        >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
        >>>>
        [-]
    ]<<

    ;; CHECK FOR RIGHT BRACKET
    [>>+<<-]>>[<<+>+>-]<         ; clone input into cell 45 ; ends at cell 45
    >[-]+<                       ; set cell 46 to 1
    --->>[-]+++++++++[<<---------->>-]<<  ; decrease cell 45 by 93
    [                            ; loop as an if ; only runs if cell 45 is 0
        >[-]                     ; this *was not* a RIGHT BRACKET ; unset cell 46
        <[-]                     ; and back to cell 45 ; drain so loop exits
    ]
    >[                           ; another if loop; only runs if cell 46 nonzero
        ;; RIGHT BRACKET FOUND ; PRINT CODE
        <<<.
        >>>
        [-]
    ]<<

    ,                        ; read more input for next iter
]


;;; FOOTER ;;;

;; BLANK LINE
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<..

;; RETURN
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
<<<<<<<<<.
>>>>>>>>>>>.
>.
<<<.
<<<.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
>>>>>>>>>.
>>>.
<<<<<<<<<<<<<.

;; END
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>.
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<.
