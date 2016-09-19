> module Main where
>
> import Protolude
>
> import Test.Tasty (TestTree, testGroup, defaultMain)
> -- import qualified Test.QuickCheck as QC
> import Test.HUnit (Assertion, (@?=))
> import Test.Tasty.HUnit (testCase)
> -- import Test.Tasty.QuickCheck (testProperty)
> import qualified Data.Text as Text
>
> import Readme.Lhs
>

hs . lhs is almost an isomorphism.

We impose a limit to the number of trailing blank lines that receive a bird.  lhs strips curl dash and dash curl from the text, and hs adds them back in whenever it discovers a boundary between (birded) code and comment.  In the stripping however, it gets forgotten where exactly the comment pragmas were placed (new line or suffix of old line).

Once having been through these normalisations, however, isomorphism should appear.  lhs and hs should then satisfy the following laws:

    -- round about isomorphisms
    (lhs . hs) . (lhs . hs) = (lhs . hs)
    (hs . lhs) . (hs . lhs) = (hs . lhs)
    lhs . hs . lhs = lhs
    hs . lhs . hs = hs

> testLhsHsIso :: Config -> [Text] -> Assertion
> testLhsHsIso cfg ts =
>     ((lhs cfg . hs cfg) . (lhs cfg . hs cfg)) ts @?= (lhs cfg . hs cfg) ts
>
> testHsLhsIso :: Config -> [Text] -> Assertion
> testHsLhsIso cfg ts =
>     ((hs cfg . lhs cfg) . (hs cfg . lhs cfg)) ts @?= (hs cfg . lhs cfg) ts
>
>
> tests :: [Text] -> [Text] -> TestTree
> tests tsHs tsLhs = testGroup "Readme.Lhs"
>     [ testCase "lhs . hs semi-iso" (testLhsHsIso (Config "" 1 Code) tsLhs)
>     , testCase "hs . lhs semi-iso" (testHsLhsIso (Config "" 1 Code) tsHs)
>     ]
>
> main :: IO ()
> main = do
>     tsHs <- Text.lines <$> readFile "test/example1.hs"
>     tsLhs <- Text.lines <$> readFile "test/example1.lhs"
>     defaultMain (tests tsHs tsLhs)
>






