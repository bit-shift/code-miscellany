A miscellany of code, either half-discarded after being written to see if it
could be done, or waiting to find a better home in a "real" project.

Stored to a public repository in case some of it turns out useful beyond my own
projects.

So far contains:
    - Some smith numbers experimentation, inspired by numberphile
      (http://youtu.be/mlqAvhjxAjo)::

        smith_numbers.py
        prime_factors.py

    - An uploader for uploading (well duh what else would an uploader do) images
      to img.tylian.net. Not really very tested, otherwise I'd put it in Toksy's
      utilities repo instead (still will when I've accounted for more failure
      cases)::

        tylian_upload.py

    - A very simple experiment with generator-based cooperative multitasking,
      using yield to pass out a result and the number of cycles the generator
      wants to "sleep" for ("yield result" is assumed to be equal to
      "yield result, 1")::

        cooperative_multitasking.py

    - A proof-of-concept module implementing the Luhn check digit algorithm::

        luhn.py

    - A module with a basic Vignere cipher implementation::

        vignere.py

    - A quick proof-of-concept for checking for intentionally empty/missing
      arguments in shell scripts::

        case_test.sh

    - A quicksort implementation with a small runnable stub for maybe profiling
      later::

        quicksort.py

    - A short script to search words provided on stdin for matching against the
      set of characters passed on the command-line. Could be used for anagram
      searching by piping in a dictionary and grepping for only the lines where
      there were 0 leftover chars::

        word_lookup.py

    - A script checking the numbers (corresponding to Pokémon) output by a "find
      out which Pokémon you are"-type algorithm I saw on tumblr
      (http://minti-pony.tumblr.com/post/51523769494/lyricamod-zachmorristoonart-pettyartist),
      to determine if they're well-distributed (no). Outputs the frequency of
      different Pokémon for different name lengths and birthdays, and then a
      sorted list of total occurences of each at the end::

       pokemon_graphing.py

    - A toy brainfuck interpreter/transpiler, and some sample programs::

       brainfuck.py       # The interpreter/transpiler.
       cat.bf             # Minimal example, mimics behaviour of cat utility.
       helloworld.bf      # A dumb approach to Hello World. Also tests UTF-8.
       betterhello.bf     # A cleaner approach to Hello World.
       brainfuck.bf       # A brainfuck transpiler in brainfuck.

    - A script for finding years in which no digit appears twice, grouping them
      by ranges and displaying range length where more than one consecutive year
      has that property. Initially coded directly in the interpreter for a post
      (http://bitshift-pony.tumblr.com/post/64129777247/imdoingstuffandthingslori),
      but since cleaned up for easier reading::

       no_repeat_years.py

    - A script for converting Nokia Composer notation into the sequence of
      keypresses to enter it into an actual handset::

       nokia_composer.py

    - A toy ocaml program for (en|de)coding files using the Numkrot cipher::

       numkrot.ml

    - A tail-recursive FizzBuzz printer in OCaml, with the ability to take an
      iteration count on the command-line::

       fizzbuzz.ml

    - A toy OCaml program for finding all permutations of the sequence of
      arguments provided to it (so for example, "a b c" -> "a b c", "a c b",
      "b a c", "b c a", "c a b", "c b a")::

       permutations.ml

    - A little shell script to store quick notes (aliased as @="noglob jotnote"
      in my environment for very fast noting)::

       jotnote.sh

    - A short Nimrod program to wrap jotnote notes to 80 chars per line::

       wrapnote.nim   # as an aside, nimrod's standard library is awesome

    - A proof-of-concept for just how far nimrod's case-insensitivity goes::

       names_test.nim

    - A very small reimplementation of dos2unix in C::

       d2u.c

    - A somewhat larger reimplementation of tr in Python::

       tr.py

    - Hyperoperations (https://en.wikipedia.org/wiki/Hyperoperation) in Python::

       hyperop.py

    - A few experiments with writing increasingly generic FizzBuzz programs
      in Haskell::

       fizzbuzz.hs
       genericfizzbuzz.hs

    - A pretty nice alternative implementation of wc in Haskell::

       WC.hs

    - A SLOC counter written in Haskell, with various heuristics for guessing
      filetypes and ignoring comment/etc. lines based on filetype::

       sloc.hs
