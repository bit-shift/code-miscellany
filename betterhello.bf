>++[<+++++>-][-]<            ; set '\n' using a counter; zero the counter

[>>+<<-]>>[<<+>+>-]<         ; clone '\n'
++>+++++[<++++>-][-]<        ; increase to ' ' with a counter; zero the counter

[>>+<<-]>>[<<+>+>-]<         ; clone '  '
>++++++[<++++++>-][-]<       ; increase to 'D' with a counter; zero the counter

[>>+<<-]>>[<<+>+>-]<         ; clone 'D'
+                            ; increase to 'E'

[>>+<<-]>>[<<+>+>-]<         ; clone 'E'
+++                          ; increase to 'H'

[>>+<<-]>>[<<+>+>-]<         ; clone 'H'
++++                         ; increase to 'L'

[>>+<<-]>>[<<+>+>-]<         ; clone 'L'
+++                          ; increase to 'O'

[>>+<<-]>>[<<+>+>-]<         ; clone 'O'
+++                          ; increase to 'R'

[>>+<<-]>>[<<+>+>-]<         ; clone 'R'
+++++                        ; increase to 'W'

>++++++++++[<                ; loop on a 10 counter
    <<<<.                    ; move to H ; print
    <.                       ; move to E ; print
    >>.                      ; move to L ; print
    .                        ; stay on L ; print
    >.                       ; move to O ; print
    <<<<<.                   ; move to space ; print
    >>>>>>>.                 ; move to W ; print
    <<.                      ; move to O ; print
    >.                       ; move to R ; print
    <<.                      ; move to L ; print
    <<<.                     ; move to D ; print
    <<.                      ; move to newline ; print
    >>>>>>>>                 ; move back to W for next iter
>-]                          ; other end of the 10 loop
