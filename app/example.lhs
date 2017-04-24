```include
other/header.md
```

<blockquote cite>
The language in which we express our ideas has a strong influence on our thought processes. ~ Knuth
</blockquote>

readme-lhs
===

[Readme-lhs](https://github.com/tonyday567/readme-lhs) is an example of mixing [markdown](https://daringfireball.net/projects/markdown/syntax) and haskell code in literate style, aiming for rapid development from initial idea to rendered description. The project:

- aims for a minimal but complete startup phase for a haskell project.
- Targets [pandoc](http://pandoc.org/) for document conversion. `-f markdown+lhs` is a georgeous rendering of a .lhs file.
- utilises stack for a tight compilation loop, from initial code to rendered html, including computational results (via [pandoc-include](https://hackage.haskell.org/package/pandoc-include)).
- Targets [github pages](https://help.github.com/articles/user-organization-and-project-pages/) for immediate publication on `git push`.

The `app/example.lhs` is then a one-stop shop for links, ideas, app, tester and example holder.

compilation recipe
---

~~~
stack build --test --exec "$(stack path --local-install-root)/bin/readme-lhs-test-example" --exec "$(stack path --local-bin)/pandoc -f markdown+lhs -i app/example.lhs -t html -o index.html --filter pandoc-include --mathjax" --file-watch
~~~

The above `recipe`, taking advantage of stack composability, builds the project, runs the test, renders this file as html, and then watches for file changes.  Pandoc and pandoc-include are assumed to be installed via stack, so you might have to:

~~~
stack install pandoc
stack install pandoc-include
~~~

template
---

The bare bones of this process is available as a stack template:

~~~
stack new project-name readme-lhs
cd project-name
stack build
$(stack path --local-install-root)/bin/readme-lhs-example
~~~

The `other/readme-lhs.hsfiles` can always be edited, renamed etc and dropped into a directory, and stack will find it.

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
\end{code}

[pragmas](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html)
---

\begin{code}
-- doctest doesn't look at the cabal file, so you need pragmas here
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
\end{code}

[libraries](https://www.stackage.org/)
---

- [protolude](https://www.stackage.org/package/protolude)
- [optparse-generic](https://www.stackage.org/package/optparse-generic)

\begin{code}
import Protolude
import Options.Generic
\end{code}

code
---

- [hoogle](https://www.stackage.org/package/hoogle)

\begin{code}
data Opts w = Opts
    { number :: w ::: Maybe Integer <?> "The number you want to product to"
    }
    deriving (Generic)

instance ParseRecord (Opts Wrapped)

main :: IO ()
main = do
    o :: Opts Unwrapped <- unwrapRecord "an example app for readme-lhs"
    let n = fromMaybe 10 (number o)
    let answer = product [1..n]
    putStrLn (show answer <> " 👍" :: Text)
    writeFile "other/answer.md"
        ("$\\prod_{i=1}^{" <> show n <> "} i = " <>
         show answer <> "$")
\end{code}

output
---

```include
other/answer.md
```

tests
---

[doctest](https://www.stackage.org/package/doctest) is a lightweight test framework that checks example code.

\begin{code}
-- | doctests
-- >>> let n = 10
-- >>> product [1..n]
-- 3628800
\end{code}

***

<div class="footer">
Powered by [haskell](https://haskell-lang.org/), [stack](https://docs.haskellstack.org/en/stable/README/) and [pandoc](http://pandoc.org/).
</div>
