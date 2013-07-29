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
