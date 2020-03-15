{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Readme.Lhs
  ( para,
    plain,
    table,
    table',
    code,
    link,
    linkTooltip,
    badge,
    image,
    Flavour (..),
    readPandoc,
    renderMarkdown,
    renderHtml,
    Output (..),
    OutputMap,
    output,
    insertOutput,
    runOutput,
    tweakHaskellCodeBlock,
    Block (..),
    module Text.Pandoc.Definition,
    Alignment (..),
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.Bool
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text as Text
import Data.Text.Lazy (toStrict)
import qualified Data.Text.IO as Text
import Text.Pandoc
import Text.Pandoc.Definition
import Prelude
import qualified Text.Blaze.Html.Renderer.Text as Blaze

-- | output can be native pandoc, or text that replaces or inserts into the output code block.
data Output = Native [Block] | Replace Text | Fence Text

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

-- | create a link
-- >>> link "test" "link"
-- Link ("",[],[]) [Str "test"] ("link","")
link :: Text -> Text -> Inline
link name url = Link ("", [], []) [Str (Text.unpack name)] (Text.unpack url, "")

-- | create a link
-- >>> linkTooltip "test" "link" "tooltip"
-- Link ("",[],[]) [Str "test"] ("link","tooltip")
linkTooltip :: Text -> Text -> Text -> Inline
linkTooltip name url tooltip = Link ("", [], []) [Str (Text.unpack name)] (Text.unpack url, Text.unpack tooltip)

-- | create an image link
-- >>> image "test" "imagelink.svg"
-- Image ("",[],[]) [Str "test"] ("imagelink.svg","")
image :: Text -> Text -> Inline
image name url = Image ("", [], []) [Str (Text.unpack name)] (Text.unpack url, "")

-- | create a badge link
-- >>> badge "Build Status" "https://travis-ci.org/tonyday567/readme-lhs.svg" "https://travis-ci.org/tonyday567/readme-lhs"
-- Link ("",[],[]) [Image ("",[],[]) [Str "Build Status"] ("https://travis-ci.org/tonyday567/readme-lhs.svg","")] ("https://travis-ci.org/tonyday567/readme-lhs","")
badge :: Text -> Text -> Text -> Inline
badge label badge' url = Link ("", [], []) [Image ("", [], []) [Str (Text.unpack label)] (Text.unpack badge', "")] (Text.unpack url, "")

-- | create a table from text
-- >>> table "an example table" ["first column", "second column"] [AlignLeft, AlignRight] [0,0] [["first row", "1"], ["second row", "1000"]]
-- Table [Str "an example table"] [AlignLeft,AlignRight] [0.0,0.0] [[Para [Str "first column"]],[Para [Str "second column"]]] [[[Para [Str "first row"]],[Para [Str "1"]]],[[Para [Str "second row"]],[Para [Str "1000"]]]]
table :: Text -> [Text] -> [Alignment] -> [Int] -> [[Text]] -> Block
table caption hs as ws rs =
  Table
    (inline caption)
    as
    (fromIntegral <$> ws)
    ((: []) . para <$> hs)
    (fmap ((: []) . para) <$> rs)

-- | create a table from inlines
-- >>> table' "an example table" [Str "first column", Str "second column"] [AlignLeft, AlignRight] [0,0] [[Str "first row", Str "1"], [Str "second row", Str "1000"]]
-- Table [Str "an example table"] [AlignLeft,AlignRight] [0.0,0.0] [[Para [Str "first column"]],[Para [Str "second column"]]] [[[Para [Str "first row"]],[Para [Str "1"]]],[[Para [Str "second row"]],[Para [Str "1000"]]]]
table' :: Text -> [Inline] -> [Alignment] -> [Int] -> [[Inline]] -> Block
table' caption hs as ws rs =
  Table
    (inline caption)
    as
    (fromIntegral <$> ws)
    ((: []) . Para . (: []) <$> hs)
    (fmap ((: []) . Para . (: [])) <$> rs)

-- | code identifier classes text
-- >>> code "name" ["sourceCode", "literate", "haskell"] "x = 1\n"
-- CodeBlock ("name",["sourceCode","literate","haskell"],[]) "x = 1\n"
code :: Text -> [Text] -> Text -> Block
code name classes =
  CodeBlock (Text.unpack name, Text.unpack <$> classes, []) . Text.unpack

-- | use LHS when you want to just add output to a *.lhs
-- | use GitHubMarkdown for rendering code and results on github
data Flavour = GitHubMarkdown | LHS | Html

-- | exts LHS is equivalent to 'markdown+lhs'
--  exts GitHubMarkdown is equivalent to 'gfm'
exts :: Flavour -> Extensions
exts LHS = enableExtension Ext_literate_haskell $ getDefaultExtensions "markdown"
exts GitHubMarkdown =
  enableExtension
    Ext_fenced_code_attributes
    githubMarkdownExtensions
exts Html = getDefaultExtensions "html"

-- |
-- literate haskell code blocks comes out of markdown+lhs to native pandoc with the following classes:
--
-- ["sourceCode","literate","haskell"]
--
--   and then conversion to github flavour gives:
--
-- ``` sourceCode
-- ```
--
-- which doesn't lead to nice code highlighting on github (and elsewhere).  This function tweaks the list so that ["haskell"] is the class, and it all works.
tweakHaskellCodeBlock :: Block -> Block
tweakHaskellCodeBlock (CodeBlock (id', cs, kv) b) =
  CodeBlock (id', bool cs ["haskell"] ("haskell" `elem` cs), kv) b
tweakHaskellCodeBlock x = x

-- | read a file into the pandoc AST
readPandoc :: FilePath -> Flavour -> IO (Either PandocError Pandoc)
readPandoc fp f = do
  t <- liftIO $ readFile fp
  runIO $ readMarkdown (def :: ReaderOptions) {readerExtensions = exts f} (Text.pack t)

-- | render a pandoc AST
-- >>> renderMarkdown GitHubMarkdown (Pandoc mempty [Table [] [] [] [] [[[Para [Str "1"]],[Para [Str "2"]]]]])
-- Right "|     |     |\n|-----|-----|\n| 1   | 2   |"
renderMarkdown :: Flavour -> Pandoc -> Either PandocError Text
renderMarkdown f (Pandoc meta bs) =
  runPure $
    writeMarkdown
      (def :: WriterOptions) {writerExtensions = exts f}
      (Pandoc meta (tweakHaskellCodeBlock <$> bs))

-- Blaze.renderHtml <$> (runPure $ writeHtml5 (def {writerExtensions = enableExtension Ext_multiline_tables (getDefaultExtensions "html")}) (Pandoc mempty [Table [Str "test"] [AlignLeft,AlignLeft] [0.0,0.0] [[Plain [Str "first",Space,Str "column"]],[Plain [Str "second",Space,Str "column"]]] [[[Plain [Str "1"]]  ,[Plain [Str "2"]]] ,[[Plain [Str "3"]]  ,[Plain [Str "4"]]]]]

-- | render a pandoc AST to Html
-- Note that text align for a Table cannot be blank when rendering to html
-- >>> Blaze.renderHtml <$> (runPure $ writeHtml5 (def {writerExtensions = (getDefaultExtensions "html")}) (Pandoc mempty [Table [] [AlignLeft,AlignLeft] [] [] [[[Plain [Str "1"]]  ,[Plain [Str "2"]]]]]))
-- Right "<table>\n<tbody>\n<tr class=\"odd\">\n<td style=\"text-align: left;\">1</td>\n<td style=\"text-align: left;\">2</td>\n</tr>\n</tbody>\n</table>"
--
renderHtml :: Flavour -> Pandoc -> Either PandocError Text
renderHtml f (Pandoc meta bs) =
  runPure $ do
    h <- writeHtml5
      (def :: WriterOptions) {writerExtensions = exts f}
      (Pandoc meta (tweakHaskellCodeBlock <$> bs))
    pure $ toStrict $ Blaze.renderHtml h

insertOutput :: OutputMap -> Block -> [Block]
insertOutput m b = case b of
  b'@(CodeBlock (id', classes, kv) _) ->
    bool
      [b']
      ( maybe
          [CodeBlock (id', classes, kv) mempty]
          ( \x ->
              maybe
                [CodeBlock (id', classes, kv) mempty]
                ( \case
                    Fence t -> [CodeBlock (id', classes, kv) . Text.unpack $ t]
                    Replace t -> [plain t]
                    Native bs -> bs
                )
                (Map.lookup x m)
          )
          (headMaybe . Prelude.filter ((`elem` classes) . Text.unpack) . Map.keys $ m)
      )
      ("output" `elem` classes)
  b' -> [b']
  where
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x

insertOutputs :: OutputMap -> Pandoc -> Pandoc
insertOutputs out (Pandoc meta bs) =
  Pandoc meta (mconcat $ insertOutput out <$> bs)

-- | add an output key-value pair to state
output :: (Monad m) => Text -> Output -> StateT OutputMap m ()
output k v = modify (Map.insert k v)

-- | insert outputs into a new file
runOutput ::
  (FilePath, Flavour) ->
  (FilePath, Flavour) ->
  StateT OutputMap IO () ->
  IO (Either PandocError ())
runOutput (fi, flavi) (fo, flavo) out = do
  m <- execStateT out Map.empty
  p <- readPandoc fi flavi
  let w = do
        p' <- insertOutputs m <$> p
        case flavo of
          Html -> renderHtml flavo p'
          _ -> renderMarkdown flavo p'
  either (pure . Left) (\t -> Text.writeFile fo t >> pure (Right ())) w

