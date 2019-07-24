{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Readme.Lhs
  ( para
  , plain
  , table
  , code
  , Flavour(..)
  , readPandoc
  , renderMarkdown
  , Output(..)
  , OutputMap
  , output
  , runOutput
  , tweakHaskellCodeBlock
  , Block(..)
  , module Text.Pandoc.Definition
  ) where

import Protolude
import Data.Text as Text
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc
import qualified Data.Map as Map

-- | output can be native pandoc, or text that replaces or inserts into the output code block.
data Output = Native Block | Replace Text | Fence Text

type OutputMap = Map Text Output

-- | doctest
-- >>> :set -XOverloadedStrings

-- | turn text into a Pandoc Paragraph Block
-- >>> para "hello"
-- Para [Str "hello"]
para :: Text -> Block
para = Para . fmap (Str . Text.unpack) . Text.lines

-- | turn text into a Pandoc Plain Block
-- >>> plain "hello"
-- Plain [Str "hello"]
plain :: Text -> Block
plain = Plain . fmap (Str . Text.unpack) . Text.lines

-- |
-- >>> inline "two\nlines"
-- [Str "two",Str "lines"]
inline :: Text -> [Inline]
inline = fmap (Str . Text.unpack) . Text.lines

-- | table caption headers alignments widths rows
-- >>> table "an example table" ["first column", "second column"] [AlignLeft, AlignRight] [0,0] [["first row", "1"], ["second row", "1000"]]
-- Table [Str "an example table"] [AlignLeft,AlignRight] [0.0,0.0] [[Para [Str "first column"]],[Para [Str "second column"]]] [[[Para [Str "first row"]],[Para [Str "1"]]],[[Para [Str "second row"]],[Para [Str "1000"]]]]
table :: Text -> [Text] -> [Alignment] -> [Int] -> [[Text]] -> Block
table caption hs as ws rs =
      Table
      (inline caption)
      as
      (fromIntegral <$> ws)
      ((:[]) . para <$> hs)
      (fmap ((:[]) . para) <$> rs)

-- | code identifier classes text
-- >>> code "name" ["sourceCode", "literate", "haskell"] "x = 1\n"
-- CodeBlock ("name",["sourceCode","literate","haskell"],[]) "x = 1\n"
code :: Text -> [Text] -> Text -> Block
code name classes =
  CodeBlock (Text.unpack name, Text.unpack <$> classes, []) . Text.unpack

-- | use LHS when you want to just add output to a *.lhs
-- | use GitHubMarkdown for rendering code and results on github
data Flavour = GitHubMarkdown | LHS

-- | exts LHS is equivalent to 'markdown+lhs'
--  exts GitHubMarkdown is equivalent to 'gfm'
exts :: Flavour -> Extensions
exts LHS = enableExtension Ext_literate_haskell $ getDefaultExtensions "markdown"
exts GitHubMarkdown =
  enableExtension Ext_fenced_code_attributes
  githubMarkdownExtensions

{- |
literate haskell code blocks comes out of markdown+lhs to native pandoc with the following classes:

["sourceCode","literate","haskell"]

  and then conversion to github flavour gives:

``` sourceCode
```

which doesn't lead to nice code highlighting on github (and elsewhere).  This function tweaks the list so that ["haskell"] is the class, and it all works.

-}
tweakHaskellCodeBlock :: Block -> Block
tweakHaskellCodeBlock (CodeBlock (id', cs, kv) b) =
  CodeBlock (id', bool cs ["haskell"] (Protolude.any ("haskell" ==) cs), kv) b
tweakHaskellCodeBlock x = x

-- | read a file into the pandoc AST
readPandoc :: FilePath -> Flavour -> IO (Either PandocError Pandoc)
readPandoc fp f = do
  t <- liftIO $ readFile fp
  runIO $ readMarkdown (def :: ReaderOptions) { readerExtensions = exts f} t

-- | render a pandoc AST
renderMarkdown :: Flavour -> Pandoc -> Either PandocError Text
renderMarkdown f (Pandoc meta bs) =
  runPure $
  writeMarkdown (def :: WriterOptions) { writerExtensions = exts f}
  (Pandoc meta (tweakHaskellCodeBlock <$> bs))

insertOutput :: OutputMap -> Block -> Block
insertOutput m b = case b of
  (b'@ (CodeBlock (id', classes, kv) _)) ->
    bool b'
    (maybe
     (CodeBlock (id', classes, kv) mempty)
     (\x ->
        
        (maybe (CodeBlock (id', classes, kv) mempty)
         (\ot -> case ot of
              Fence t -> CodeBlock (id', classes, kv) . Text.unpack $ t
              Replace t -> plain t
              Native bs -> bs
         )
        (Map.lookup x m)))
     (headMay . Protolude.filter ((`elem` classes) . Text.unpack) . Map.keys $ m))
    ("output" `elem` classes)
  b' -> b'

-- | add an output key-value pair to state
output :: (Monad m) => Text -> Output -> StateT OutputMap m ()
output k v = modify (Map.insert k v)

-- | insert outputs into a new file
runOutput
  :: (FilePath, Flavour)
  -> (FilePath, Flavour)
  -> StateT OutputMap IO ()
  -> IO (Either PandocError ())
runOutput (fi, flavi) (fo, flavo) out = do
  m <- execStateT out Map.empty
  p <- readPandoc fi flavi
  let w = do
              p' <- fmap (\(Pandoc meta bs) -> Pandoc meta (insertOutput m <$> bs)) p
              renderMarkdown flavo p'
  either (pure . Left) (\t -> writeFile fo t >> pure (Right ())) w
