readme-lhs
===

[![Build Status](https://travis-ci.org/tonyday567/readme-lhs.svg)](https://travis-ci.org/tonyday567/readme-lhs) [![Hackage](https://img.shields.io/hackage/v/readme-lhs.svg)](https://hackage.haskell.org/package/readme-lhs)

<blockquote cite>
The language in which we express our ideas has a strong influence on our
thought processes ~ Knuth
</blockquote>

Support for literate programming in haskell including:

- conversion between *.lhs and *.hs formats.
- insertion of program output into *.lhs and *.md for fast feedback in development.
- a simple wrapper for pandoc functionality.
- some simple number formatting
- a stack template, `readme-lhs`

example insert
---

``` {.output .example}
```

template
===

A bare bones stack template is located in
[other/readme-lhs.hsfiles](other/readme-lhs.hsfiles). It contains what
you need to quickly get started with literate programming and enjoy a rapid workflow

Place this in a local directory and then:

```
stack new xyzzy readme-lhs
cd xyzzy
stack build
```

will get you bootstrapped and ready to hack!

development
===

`stack build --test --exec "$(stack path --local-install-root)/bin/readme-lhs-example --file-watch`.
