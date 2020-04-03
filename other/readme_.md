[readme-lhs](https://tonyday567.github.io/readme-lhs/index.html) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.svg)](https://travis-ci.org/tonyday567/readme-lhs)
================================================================================================================================================================================

<blockquote cite>
The language in which we express our ideas has a strong influence on our
thought processes. Knuth
</blockquote>

This is how I start a new haskell library refactor. I pick a new ghc version, and a new stack lts, and set this project up with no compile warts.  This gives me the full pandoc tree, which is a great base to get a fast workflow loop going for the repo you've created.


example

``` {.output .example}
```

NumHask.Space

``` {.output .NumHask.Space}
```

NumHask.Array

``` {.output .NumHask.Array}
```


Box

``` {.output .Box}
```


web-rep

``` {.output .web-rep}
```

chart-svg

``` {.output .chart-svg}
```








template
========

A bare bones stack template is located in
[other/readme-lhs.hsfiles](other/readme-lhs.hsfiles). It contains what
you need to quickly get started with literate programming.

workflow
--------

    stack build --test --exec "$(stack path --local-install-root)/bin/readme-lhs-example" --file-watch
