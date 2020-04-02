[readme-lhs](https://tonyday567.github.io/readme-lhs/index.html) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.svg)](https://travis-ci.org/tonyday567/readme-lhs)
================================================================================================================================================================================

<blockquote cite>
The language in which we express our ideas has a strong influence on our
thought processes. Knuth
</blockquote>

This is how I start a new haskell library refactor. I pick a new ghc version, and a new stack lts, and set this project up with no compile warts.  This gives me the full pandoc tree, which is a great base to get a fast workflow loop going for the repo you've created.


[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
--------------------------------------------------------------------------------------------------------

``` {.haskell}
{-# OPTIONS_GHC -Wall #-}
```

and in cabal file:

```
ghc-options:
    -Wall
    -Wcompat
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wredundant-constraints
```

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
------------------------------------------------------------------------------------

``` {.haskell}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
```

I have:
  - NegativeLiterals
  - OverloadedStrings
  - UnicodeSyntax

... permanently turned on in the cabal file


[libraries](https://www.stackage.org/)
--------------------------------------

-   [protolude](https://www.hackage.org/package/protolude)
-   [readme-lhs](https://www.hackage.org/package/readme-lhs)

``` {.haskell}
import Protolude
import Readme.Lhs
```

code
----

-   [hoogle](https://www.stackage.org/package/hoogle)

``` {.haskell}
main :: IO ()
main = do
  let n = 10
  let answer = product [1..n::Integer]
  void $ runOutput ("example.lhs", LHS) ("readme.md", GitHubMarkdown) $ do
    output "example1" (Fence "Simple example of an output")
```

``` {.output .example}
Simple example of an output
```

``` {.haskell}
    output "example2" (Fence (show answer))
```

10! is equal to:

``` {.output .example2}
3628800
```

As well as fenced output, output can be Text that replaces the {.output}
code block

``` {.haskell}
    output "example3" (Replace "Fenced code block was overwritten")
```

Fenced code block was overwritten

or be native pandoc.

``` {.haskell}
    output "example4" (Native [BulletList [[plain "a"], [plain "bullet"], [plain "list"]]])
```

-   a
-   bullet
-   list

Output that doesn’t exist is simply cleared.

Fenced code block was overwritten

Technicals
==========

This is an example of mixing literate haskell with markdown, and in
using readme-lhs. The file is composed of several elements:

-   literate haskell. Bird-tracks are used, as the alternative lhs
    method is latex. Pandoc can read this, but defaults to bird tracks
    when rendering `markdown+lhs`.

-   markdown. All non bird-tracked lines are considered to be markdown.
    It’s probably incompatible with haddock. This might be easily
    fixable.

-   fenced code blocks with an output class, which are used to insert
    computation results. The fenced code blocks look like:

    \`\`\`{.output .example} \`\`\`

As it currently stands, ghc cannot read a file with fenced code-blocks
that look like:

    \```haskell
    \```

Given this, a file cannot be both a valid haskell file, and a markdown
file that is rendered nicely by github. This would resolve with adoption
of the [literate markdown ghc
proposal](https://gitlab.haskell.org/ghc/ghc/wikis/literate-markdown).

template
========

A bare bones stack template is located in
[other/readme-lhs.hsfiles](other/readme-lhs.hsfiles). It contains what
you need to quickly get started with literate programming.

workflow
--------

    stack build --test --exec "$(stack path --local-install-root)/bin/readme-lhs-example" --file-watch
