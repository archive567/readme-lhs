> {-# OPTIONS_GHC -Wall #-}
> module Main where
>
> import Protolude hiding (print)
>
> import Test.Tasty (TestTree, testGroup, defaultMain)
> import Test.HUnit (Assertion, (@?=))
> import Test.Tasty.HUnit (testCase)
> import qualified Data.Text as Text
>
> import Readme.Lhs
>

parse . print is almost an isomorphism.

Forgetting whether comment marks were on a new line or embedded in with the comment text lines, and ensuring enough spaces in lhs comment sections to avoid unlit causes some initial style to be lost.

Once having been through these normalisations, however, isomorphism should appear.  print and parse should then satisfy the following laws:

    (parse Lhs . print Lhs) = id -- printid
    -- round about isomorphism
    (print Lhs . parse Lhs) . (print Lhs . parse Lhs) = (print Lhs . parse Lhs) -- parseid

> testPrintid :: Format -> [Block] -> Assertion
> testPrintid s f =
>     (parse s . print s) f @?= f
>
> testParseid :: Format -> [Text] -> Assertion
> testParseid s ts =
>     ((print s . parse s) . (print s . parse s)) ts @?= (print s . parse s) ts
>
> tests :: [Text] -> [Text] -> TestTree
> tests tsHs tsLhs = testGroup "Readme.Lhs"
>     [ testCase "print parse iso - lhs" (testPrintid Lhs (parse Lhs tsLhs))
>     , testCase "parse print pseudo-iso - lhs" (testParseid Lhs tsLhs)
>     , testCase "print parse iso - hs" (testPrintid Hs (parse Hs tsHs))
>     , testCase "parse print pseudo-iso - hs" (testParseid Hs tsHs)
>     ]
> 
> main :: IO ()
> main = do
>     tsHs <- Text.lines <$> readFile "test/example1.hs"
>     tsLhs <- Text.lines <$> readFile "test/example1.lhs"
>     defaultMain (tests tsHs tsLhs)
>





