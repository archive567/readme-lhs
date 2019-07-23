{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Readme.Lhs where

import Protolude
import Data.Text as Text
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc
import qualified Data.Map as Map

type Output = Map Text Text

para :: Text -> Block
para = Para . fmap (Str . Text.unpack) . Text.lines

plain :: Text -> Block
plain = Para . fmap (Str . Text.unpack) . Text.lines

inline :: Text -> [Inline]
inline = fmap (Str . Text.unpack) . Text.lines

-- | table caption headers alignments widths rows
table :: Text -> [Text] -> [Alignment] -> [Int] -> [[Text]] -> Block
table caption hs as ws rs =
      Table
      (inline caption)
      as
      (fromIntegral <$> ws)
      ((:[]) . para <$> hs)
      (fmap ((:[]) . para) <$> rs)

code :: Text -> [Text] -> Text -> Block
code name classes =
  CodeBlock (Text.unpack name, Text.unpack <$> classes, []) . Text.unpack


data Flavour = GitHubMarkdown | LHS

exts :: Flavour -> Extensions
exts LHS = enableExtension Ext_literate_haskell $ getDefaultExtensions "markdown"
exts GitHubMarkdown = githubMarkdownExtensions


{-
literate haskell code blocks comes out of markdown+lhs to native pandoc with the following classes:

["sourceCode","literate","haskell"]

  and then conversion to github flavour gives:

``` sourceCode
```

hich doesn't lead to nice code highlighting on github (and elsewhere).  This function tweaks the list so that ["haskell"] is the class, and it all works.

-}
tweakHaskellCodeBlock :: Block -> Block
tweakHaskellCodeBlock (CodeBlock (id', cs, kv) b) =
  CodeBlock (id', bool cs ["haskell"] (Protolude.any ("haskell" ==) cs), kv) b
tweakHaskellCodeBlock x = x

-- disableExtension Ext_fenced_code_attributes $ 

readPandoc :: FilePath -> Flavour -> IO (Either PandocError Pandoc)
readPandoc fp f = do
  t <- liftIO $ readFile fp
  runIO $ readMarkdown (def :: ReaderOptions) { readerExtensions = exts f} t

renderMarkdown :: Flavour -> Pandoc -> Either PandocError Text
renderMarkdown f (Pandoc meta bs) =
  runPure $
  writeMarkdown (def :: WriterOptions) { writerExtensions = exts f}
  (Pandoc meta (tweakHaskellCodeBlock <$> bs))

insertOutput :: Output -> Block -> Block
insertOutput m b = case b of
  (b'@ (CodeBlock (id', classes, kv) _)) ->
    bool b'
    (maybe
     (CodeBlock (id', classes, kv) mempty)
     (\x -> CodeBlock (id', classes, kv) . maybe mempty Text.unpack $ Map.lookup x m)
     (headMay . Protolude.filter ((`elem` classes) . Text.unpack) . Map.keys $ m))
    ("output" `elem` classes)
  b' -> b'

output :: (Monad m) => Text -> Text -> StateT (Map Text Text) m ()
output k v = modify (Map.insert k v)

runOutput
  :: (FilePath, Flavour)
  -> (FilePath, Flavour)
  -> StateT Output IO ()
  -> IO (Either PandocError ())
runOutput (fi, flavi) (fo, flavo) out = do
  m <- execStateT out Map.empty
  p <- readPandoc fi flavi
  let w = do
              p' <- fmap (\(Pandoc meta bs) -> Pandoc meta (insertOutput m <$> bs)) p
              renderMarkdown flavo p'
  either (pure . Left) (\t -> writeFile fo t >> pure (Right ())) w



