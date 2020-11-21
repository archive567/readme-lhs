{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

-- | Helpers for converting between lhs and hs files.
--
module Readme.Convert
  ( Section (..),
    Block (..),
    Format (..),
    bird,
    normal,
    parseHs,
    printHs,
    parseLhs,
    printLhs,
    parse,
    print,
    lhs2hs,
    hs2lhs
  )
where

import qualified Control.Foldl as L
import qualified Data.Attoparsec.Text as A
import qualified Data.List as List
import NumHask.Prelude hiding (print)

-- | Type of file section
data Section = Code | Comment deriving (Show, Eq)

-- | A native section block.
data Block = Block Section [Text] deriving (Show, Eq)

-- | *.lhs bird style
bird :: A.Parser Block
bird =
  (\x -> Block Code [x]) <$> ("> " *> A.takeText)
    <|> (\_ -> Block Code [""]) <$> (">" *> A.takeText)
    <|> (\x -> Block Comment [x]) <$> A.takeText

-- | Parse an lhs-style block of text in
parseLhs :: [Text] -> [Block]
parseLhs text = L.fold (L.Fold step begin done) $ A.parseOnly bird <$> text
  where
    begin = (Block Code [], [])
    done (Block _ [], out) = unlit' out
    done (block, out) = unlit' $ out <> [block]
    unlit' ss =
      ( \(Block s ts) ->
          case s of
            Comment -> Block s (unlit ts)
            Code -> Block s ts
      )
        <$> ss
    step x (Left _) = x
    step (Block s ts, out) (Right (Block s' ts')) =
      if
        | s == s' -> (Block s (ts <> ts'), out)
        | otherwise -> case ts of
          [] -> (Block s' ts, out)
          _ -> (Block s' ts', out <> [Block s ts])
    unlit [] = [""]
    unlit [""] = [""]
    unlit xs =
      if
        | (head xs == Just "") && (head (reverse xs) == Just "") ->
          List.init $ List.tail xs
        | (head xs == Just "") ->
          List.tail xs
        | (head (reverse xs) == Just "") ->
          List.init xs
        | otherwise ->
          xs

-- | Convert a block of code into lhs.
printLhs :: [Block] -> [Text]
printLhs ss =
  mconcat $
    ( \(Block s ts) ->
        case s of
          Code -> ("> " <>) <$> ts
          Comment -> lit ts
    )
      <$> ss
  where
    lit [] = [""]
    lit [""] = [""]
    lit xs =
      bool [""] [] (head xs == Just "")
        <> xs
        <> bool [""] [] (List.last xs == "")

-- | Parse a .hs
--
-- Normal code (.hs) is parsed where lines that are continuation of a section (neither contain clues as to whether code or comment) are output as Nothing, and the clues as to what the current and next section are is encoded as Just (current, next).
normal :: A.Parser (Maybe (Section, Section), [Text])
normal =
  -- Nothing represents a continuation of previous section
  (Nothing, [""]) <$ A.endOfInput
    <|>
    -- exact matches include line removal
    (Just (Comment, Comment), []) <$ ("{-" *> A.endOfInput)
    <|> (Just (Comment, Code), []) <$ ("-}" *> A.endOfInput)
    <|>
    -- single line braced
    (\x -> (Just (Code, Code), ["{-" <> x <> "-}"]))
      <$> ("{-" *> (pack <$> A.manyTill' A.anyChar "-}"))
    <|>
    -- pragmas
    (\x -> (Just (Code, Code), ["{-#" <> x])) <$> ("{-#" *> A.takeText)
    <|> (\x -> (Just (Code, Code), [x])) <$> (pack <$> A.manyTill' A.anyChar "#-}")
    <|>
    -- braced start of multi-line comment (brace is stripped)
    (\x -> (Just (Comment, Comment), [x])) <$> ("{-" *> A.takeText)
    <|>
    -- braced end of multi-line comment (brace is stripped)
    (\x -> (Just (Comment, Code), [x])) <$> (pack <$> A.manyTill' A.anyChar "-}")
    <|>
    -- everything else a continuation and verbatim
    (\x -> (Nothing, [x])) <$> A.takeText

-- | Parse assuming a hs block of code
parseHs :: [Text] -> [Block]
parseHs text = L.fold (L.Fold step begin done) $ A.parseOnly normal <$> text
  where
    begin = (Block Code [], [])
    done (Block _ [], out) = out
    done (buff, out) = out <> [buff]
    step x (Left _) = x
    step (Block s ts, out) (Right (Just (this, next), ts')) =
      if
        | null (ts <> ts') -> (Block next [], out)
        | this == s && next == s -> (Block s (ts <> ts'), out)
        | this /= s -> (Block this ts', out <> [Block s ts])
        | otherwise -> (Block next [], out <> [Block s (ts <> ts')])
    step (Block s ts, out) (Right (Nothing, ts')) =
      if
        | null (ts <> ts') -> (Block s [], out)
        | otherwise -> (Block s (ts <> ts'), out)

-- | Print a block of code to hs style
printHs :: [Block] -> [Text]
printHs ss =
  mconcat $
    ( \(Block s ts) ->
        case s of
          Code -> ts
          Comment -> ["{-"] <> ts <> ["-}"]
    )
      <$> ss

-- | just in case there are ever other formats (YAML haskell anyone?)
data Format = Lhs | Hs

-- | Print
print :: Format -> [Block] -> [Text]
print Lhs f = printLhs f
print Hs f = printHs f

-- | Parse
parse :: Format -> [Text] -> [Block]
parse Lhs f = parseLhs f
parse Hs f = parseHs f

-- | Convert a file from lhs to hs
lhs2hs :: FilePath -> IO ()
lhs2hs fp = do
  t <- readFile (fp <> ".lhs")
  writeFile (fp <> ".hs") $ unlines $ print Hs $ parse Lhs $ lines t

-- | Convert a file from hs to lhs
hs2lhs :: FilePath -> IO ()
hs2lhs fp = do
  t <- readFile (fp <> ".hs")
  writeFile (fp <> ".lhs") $ unlines $ print Lhs $ parse Hs $ lines t
