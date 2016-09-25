{-# OPTIONS_GHC -Wall #-}
module Readme.Lhs
  (
      Section(..),
      Block(..),
      Format(..),
      bird,
      normal,
      parseHs,
      printHs,
      parseLhs,
      printLhs,
      parse,
      print
  ) where

import qualified Control.Foldl as L
import qualified Data.Attoparsec.Text as Text
import qualified Data.List as List
import qualified Data.Text as Text
import Protolude hiding (print)

data Section  = Code | Comment deriving (Show, Eq)
data Block = Block Section [Text] deriving (Show, Eq)

-- starting with .lhs bird style
bird :: Text.Parser Block
bird = do
    (\x -> (Block Code [x])) <$> ("> " *> Text.takeText)
    <|> (\_ -> (Block Code [""])) <$> (">" *> Text.takeText)
    <|> (\x -> (Block Comment [x])) <$> Text.takeText

parseLhs :: [Text] -> [Block]
parseLhs text = L.fold (L.Fold step begin done) $ Text.parseOnly bird <$> text
  where
    begin = ((Block Code []), [])
    done ((Block _ []),out) = unlit' $ out
    done (block,out) = unlit' $ out <> [block]
    unlit' ss = (\(Block s ts) ->
                   case s of
                     Comment -> (Block s (unlit ts))
                     Code -> (Block s ts)) <$> ss
    step x (Left _) = x
    step ((Block s ts),out) (Right (Block s' ts')) = if
        | s == s' -> ((Block s (ts<>ts')), out)
        | otherwise -> case ts of
            [] -> ((Block s' ts), out)
            _ -> ((Block s' ts'), out <> [(Block s ts)])
    unlit [] = [""]
    unlit [""] = [""]
    unlit xs = if
        | (Protolude.head xs == Just "") && (Protolude.head (reverse xs) == Just "") ->
          List.init $ List.tail xs
        | (Protolude.head xs == Just "") ->
          List.tail xs
        | (Protolude.head (reverse xs) == Just "") ->
          List.init xs
        | otherwise ->
          xs

printLhs :: [Block] -> [Text]
printLhs ss = Protolude.mconcat $
    (\(Block s ts) ->
       case s of
         Code -> ("> " <>) <$> ts
         Comment -> lit ts)
    <$> ss
  where
    lit [] = [""]
    lit [""] = [""]
    lit xs =
        (if (Protolude.head xs == Just "") then [] else [""]) <>
        xs <>
        (if (List.last xs == "") then [] else [""])

-- coming from hs
-- normal code (.hs) is parsed where lines that are continuation of a section (neither contain clues as to whether code or comment) are output as Nothing, and the clues as to what the current and next section are is encoded as Just (current, next).
normal :: Text.Parser (Maybe (Section, Section), [Text])
normal = do
    -- Nothing represents a continuation of previous section
    (\_ -> (Nothing, [""])) <$> Text.endOfInput <|>
     -- exact matches include line removal
     (\_ -> (Just (Comment, Comment), [])) <$> ("{-" *> Text.endOfInput) <|>
     (\_ -> (Just (Comment, Code), [])) <$> ("-}" *> Text.endOfInput) <|>
     -- single line braced
     (\x -> (Just (Code, Code), ["{-" <> x <> "-}"])) <$>
         ("{-" *> (Text.pack <$> Text.manyTill' Text.anyChar "-}")) <|>
     -- pragmas
     (\x -> (Just (Code, Code), ["{-#" <> x])) <$> ("{-#" *> Text.takeText) <|>
     (\x -> (Just (Code, Code), [x])) <$> (Text.pack <$> Text.manyTill' Text.anyChar "#-}") <|>
     -- braced start of multi-line comment (brace is stripped)
     (\x -> (Just (Comment, Comment), [x])) <$> ("{-" *> Text.takeText) <|>
     -- braced end of multi-line comment (brace is stripped)
     (\x -> (Just (Comment, Code), [x])) <$> (Text.pack <$> Text.manyTill' Text.anyChar "-}") <|>
     -- everything else a continuation and verbatim
     (\x -> (Nothing, [x])) <$> Text.takeText

parseHs :: [Text] -> [Block]
parseHs text = L.fold (L.Fold step begin done) $ Text.parseOnly normal <$> text
  where
    begin = ((Block Code []), [])
    done ((Block _ []), out) = out
    done (buff, out) = out <> [buff]
    step x (Left _) = x
    step ((Block s ts), out) (Right (Just (this, next), ts')) = if
        | ts<>ts'==[] -> ((Block next []), out)
        | this == s && next == s -> ((Block s (ts<>ts')), out)
        | this /= s -> ((Block this ts'), out <> [(Block s ts)])
        | otherwise -> ((Block next []), out <> [(Block s (ts <> ts'))])
    step ((Block s ts),out) (Right (Nothing, ts')) = if
        | ts<>ts'==[] -> ((Block s []), out)
        | otherwise -> ((Block s (ts<>ts')), out)

printHs :: [Block] -> [Text]
printHs ss = Protolude.mconcat $
    (\(Block s ts) ->
       case s of
         Code -> ts
         Comment -> ["{-"] <> ts <> ["-}"]) <$> ss

-- just in case there are ever other formats (YAML haskell anyone?)
data Format = Lhs | Hs

print :: Format -> [Block] -> [Text]
print Lhs f = printLhs f
print Hs f = printHs f

parse :: Format -> [Text] -> [Block]
parse Lhs f = parseLhs f
parse Hs f = parseHs f
