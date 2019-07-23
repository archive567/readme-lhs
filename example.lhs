---
pagetitle: readme-lhs
---

[readme-lhs](https://tonyday567.github.io/readme-lhs/index.html) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.svg)](https://travis-ci.org/tonyday567/readme-lhs)
===

<blockquote cite>
The language in which we express our ideas has a strong influence on our thought processes. ~ Knuth
</blockquote>

This is an example of mixing literate haskell with markdown, and in using Readme.Lhs.  The file is composed of several elements:

- literate haskell. Bird-tracks are used, as the alternative method is latex rather than markdown, which doesn't survive a pandoc round trip.
- markdown. All non bird-tracked lines are considered to be markdown.  It's probably incompatible with haddock, but this may well resolve with adoption of the recent literate markdown [ghc proposal](https://gitlab.haskell.org/ghc/ghc/wikis/literate-markdown).
- fenced code blocks with an output class, which are used to insert computation results. The fenced code blocks look like:

    ```{.output .example}
    ```

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

> {-# OPTIONS_GHC -Wall #-}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
------------------------------------------------------------------------------------

> -- doctest doesn't look at the cabal file, so you need pragmas here
> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE FlexibleInstances #-}

[libraries](https://www.stackage.org/)
--------------------------------------

-   [protolude](https://www.hackage.org/package/protolude)
-   [readme-lhs](https://www.hackage.org/package/readme-lhs)

> import Protolude
> import Readme.Lhs

code
----

-   [hoogle](https://www.stackage.org/package/hoogle)

> main :: IO ()
> main = do
>   let n = 10
>   let answer = product [1..n::Integer]
>   blocks <- readMarkdownBlocks "example.lhs"
>   _ <- runOutput "readme.md" $ do
>     output "example1" "Simple example of an output"

```{.output .example1}

```

>     output "example2" (show answer)

10! is equal to:

```{.output .example2}

```

>     pure blocks
>   pure ()
>

Output that doesn't exist is simply cleared.

``` {.output .example3}
The text inside this code block will be
  cleared on execution.
```

hsfiles writeup
===

A literate-programming friendly; tight work-flow stack template.

other/readme-lhs.hsfiles

other/batteries.hsfiles
---

This is my latest working template, overly influenced by [lexi-lambda's opinionated guide](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/). The template includes:

- some minor tweaks to protolude
- lens, foldl, formatting & text as must have libraries
- generic-lens-labels

workflow
---

```
stack build --exec "$(stack path --local-install-root)/bin/readme-lhs-example" --file-watch
```
