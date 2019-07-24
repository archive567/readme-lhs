---
pagetitle: readme-lhs
---

[readme-lhs](https://tonyday567.github.io/readme-lhs/index.html) [![Build Status](https://travis-ci.org/tonyday567/readme-lhs.svg)](https://travis-ci.org/tonyday567/readme-lhs)
===

<blockquote cite>
The language in which we express our ideas has a strong influence on our thought processes. Knuth
</blockquote>

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

> {-# OPTIONS_GHC -Wall #-}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
------------------------------------------------------------------------------------

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
>   void $ runOutput ("example.lhs", LHS) ("readme.md", GitHubMarkdown) $ do
>     output "example1" (Fence "Simple example of an output")

```{.output .example1}

```

>     output "example2" (Fence (show answer))

10! is equal to:

```{.output .example2}

```

As well as fenced output, output can be Text that replaces the {.output} code block

>     output "example3" (Replace "Fenced code block was overwritten")

```{.output .example3}
This will be replaced.
```

or be native pandoc.

>     output "example4" (Native $ BulletList [[plain "a"], [plain "bullet"], [plain "list"]])

```{.output .example4}
```

Output that doesn't exist is simply cleared.

``` {.output .example3}
The text inside this code block will be
  cleared on execution.
```

Technicals
===

This is an example of mixing literate haskell with markdown, and in using readme-lhs.  The file is composed of several elements:

- literate haskell. Bird-tracks are used, as the alternative lhs method is latex. Pandoc can read this, but defaults to bird tracks when rendering `markdown+lhs`.
- markdown. All non bird-tracked lines are considered to be markdown.  It's probably incompatible with haddock. This might be easily fixable.
- fenced code blocks with an output class, which are used to insert computation results. The fenced code blocks look like:

    \`\`\`{.output .example}
    \`\`\`

As it currently stands, ghc cannot read a file with fenced code-blocks that look like:

```
\```haskell
\```
```

Given this, a file cannot be both a valid haskell file, and a markdown file that is rendered nicely by github. This would resolve with adoption of the [literate markdown ghc proposal](https://gitlab.haskell.org/ghc/ghc/wikis/literate-markdown).

template
===

A bare bones stack template is located in [other/readme-lhs.hsfiles](other/readme-lhs.hsfiles). It contains what you need to quickly get started with literate programming.

workflow
---

```
stack build --test --exec "$(stack path --local-install-root)/bin/readme-lhs-example" --file-watch
```
