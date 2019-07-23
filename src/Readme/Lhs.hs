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
exts GitHubMarkdown = disableExtension Ext_fenced_code_attributes $ getDefaultExtensions "markdown"

--

readMarkdownBlocks :: FilePath -> Flavour -> IO [Block]
readMarkdownBlocks fp f = do
  t <- liftIO $ readFile fp
  p <- runIO $ readMarkdown (def :: ReaderOptions) { readerExtensions = exts f} t
  pure $ case p of
    Left _ -> [Null]
    Right (Pandoc _ blocks) -> blocks

output :: (Monad m) => Text -> Text -> StateT (Map Text Text) m ()
output k v = modify (Map.insert k v)

insertOutputs :: Output -> [Block] -> [Block]
insertOutputs m bs = insertOutput m <$> bs

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

runOutput
  :: FilePath
  -> Flavour
  -> StateT Output IO [Block]
  -> IO (Either PandocError (Text, Map Text Text))
runOutput fp f bs = do
  (bs', m) <- runStateT bs Map.empty
  let postbs = insertOutputs m bs'
  let res = runPure $
        writeMarkdown (def :: WriterOptions) { writerExtensions = exts f}
        (Pandoc nullMeta postbs)
  either (pure . Left) (\x -> do
                  writeFile fp x
                  pure $ Right (x, m)) res

runOutputOn
  :: FilePath
  -> Flavour
  -> StateT Output IO [Block]
  -> IO (Either PandocError (Text, Output))
runOutputOn fp f bs = do
  (bs', m) <- runStateT bs Map.empty
  let postbs = insertOutputs m bs'
  let res = runPure $
        writeNative (def :: WriterOptions) { writerExtensions = exts f}
        (Pandoc nullMeta postbs)
  either (pure . Left) (\x -> do
                  writeFile fp x
                  pure $ Right (x, m)) res


