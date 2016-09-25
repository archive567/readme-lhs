
-- initial whitespace is Code
-- single line comments are Code
{- single line braced are code -}
{-
classical wrapped multi-line, which are Comment
-}

{-# LANGUAGE NoImplicitPrelude #-}
import Protolude


{- a non-standard multi-line, where marks are on the
same line as comments -}
main :: IO ()
main = pure ()

x :: Int
x = 0

{-
end comment
-}



