code
===

[ghc options](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html#flag-reference)
---

\begin{code}
{-# OPTIONS_GHC -Wall #-}
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
  } deriving (Generic)

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

[doctest](https://www.stackage.org/package/doctest)

\begin{code}
-- | doctests
-- >>> let n = 10
-- >>> product [1..n]
-- 3628800
\end{code}
