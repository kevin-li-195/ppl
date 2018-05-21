# ppl

## Summary

ppl is a first-effort attempt at a Haskell implementation
of a type-level DSL for describing Bayesian networks in an
intuitive, graphical way. The main motivation is
the idea that graphical models should be graphical
in how they're written down, and the language in which
we express these models should reflect that.

Haskell was the language of choice because we can do
fancy type-level programming
(with the help of *many* compiler extensions).

Furthermore, embedding this DSL in an existing general-purpose
programming language affords us flexibility in how
we use this library; for instance, we implement a CSV-format
conversion here. We (and users) can easily extend functionality.

Lastly, the Haskell environment is lacking in statistical libraries,
especially ones that actually try to take advantage of the
type-system. ppl provides some compile-time guarantees relevant to
Bayesian networks (e.g. acyclicality, connected, type-safety, variables
declared before usage). Hopefully this project is interesting enough,
and has some decent ideas for further extension.

For a super brief introduction on this project, [here](writeup.pdf) is the writeup
that I did for my Bayesian stats class.
