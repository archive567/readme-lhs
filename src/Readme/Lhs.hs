{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Readme.Lhs
  ( para,
    plain,
    inline,
    table,
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

import qualified Data.Map as Map
import Text.Pandoc
import Text.Pandoc.Definition
import NumHask.Prelude hiding (link)
import qualified Text.Blaze.Html.Renderer.Text as Blaze

-- | output can be native pandoc, text that replaces or inserts into the output code block, or Html.
data Output = Native [Block] | Replace Text | Fence Text | RawHtml Text

type OutputMap = Map Text Output

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> let table1 = Table ("",[],[]) (Caption Nothing [Plain [Str "an example table"]]) [(AlignLeft,ColWidthDefault),(AlignRight,ColWidthDefault)] (TableHead ("",[],[]) [Row ("",[],[]) [Cell ("",[],[]) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [Str "first column"]]],Row ("",[],[]) [Cell ("",[],[]) AlignRight (RowSpan 1) (ColSpan 1) [Plain [Str "second column"]]]]) [TableBody ("",[],[]) (RowHeadColumns 0) [] [Row ("",[],[]) [Cell ("",[],[]) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [Str "first row"]]],Row ("",[],[]) [Cell ("",[],[]) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [Str "1"]]]],TableBody ("",[],[]) (RowHeadColumns 0) [] [Row ("",[],[]) [Cell ("",[],[]) AlignRight (RowSpan 1) (ColSpan 1) [Plain [Str "second row"]]],Row ("",[],[]) [Cell ("",[],[]) AlignRight (RowSpan 1) (ColSpan 1) [Plain [Str "1000"]]]]] (TableFoot ("",[],[]) [])

-- | turn text into a Pandoc Paragraph Block
-- >>> para "hello"
-- Para [Str "hello"]
para :: Text -> Block
para = Para . fmap Str . lines

-- | turn text into a Pandoc Plain Block
-- >>> plain "hello"
-- Plain [Str "hello"]
plain :: Text -> Block
plain = Plain . fmap Str . lines

-- |
-- >>> inline "two\nlines"
-- [Str "two",Str "lines"]
inline :: Text -> [Inline]
inline = fmap Str . lines

-- | create a link
-- >>> link "test" "link"
-- Link ("",[],[]) [Str "test"] ("link","")
link :: Text -> Text -> Inline
link name url = Link ("", [], []) [Str name] (url, "")

-- | create a link
-- >>> linkTooltip "test" "link" "tooltip"
-- Link ("",[],[]) [Str "test"] ("link","tooltip")
linkTooltip :: Text -> Text -> Text -> Inline
linkTooltip name url tooltip = Link ("", [], []) [Str name] (url, tooltip)

-- | create an image link
-- >>> image "test" "imagelink.svg"
-- Image ("",[],[]) [Str "test"] ("imagelink.svg","")
image :: Text -> Text -> Inline
image name url = Image ("", [], []) [Str name] (url, "")

-- | create a badge link
-- >>> badge "Build Status" "https://travis-ci.org/tonyday567/readme-lhs.svg" "https://travis-ci.org/tonyday567/readme-lhs"
-- Link ("",[],[]) [Image ("",[],[]) [Str "Build Status"] ("https://travis-ci.org/tonyday567/readme-lhs.svg","")] ("https://travis-ci.org/tonyday567/readme-lhs","")
badge :: Text -> Text -> Text -> Inline
badge label badge' url = Link ("", [], []) [Image ("", [], []) [Str label] (badge', "")] (url, "")

-- | create a simple table from Inlines
-- >>> table "an example table" [(Str "first column"), (Str "second column")] [AlignLeft, AlignRight] [ColWidthDefault, ColWidthDefault] [[(Str "first row"), (Str "1")], [(Str "second row"), (Str "1000")]]
-- Table ("",[],[]) (Caption Nothing [Plain [Str "an example table"]]) [(AlignLeft,ColWidthDefault),(AlignRight,ColWidthDefault)] (TableHead ("",[],[]) [Row ("",[],[]) [Cell ("",[],[]) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [Str "first column"]]],Row ("",[],[]) [Cell ("",[],[]) AlignRight (RowSpan 1) (ColSpan 1) [Plain [Str "second column"]]]]) [TableBody ("",[],[]) (RowHeadColumns 0) [] [Row ("",[],[]) [Cell ("",[],[]) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [Str "first row"]]],Row ("",[],[]) [Cell ("",[],[]) AlignLeft (RowSpan 1) (ColSpan 1) [Plain [Str "1"]]]],TableBody ("",[],[]) (RowHeadColumns 0) [] [Row ("",[],[]) [Cell ("",[],[]) AlignRight (RowSpan 1) (ColSpan 1) [Plain [Str "second row"]]],Row ("",[],[]) [Cell ("",[],[]) AlignRight (RowSpan 1) (ColSpan 1) [Plain [Str "1000"]]]]] (TableFoot ("",[],[]) [])
table :: Text -> [Inline] -> [Alignment] -> [ColWidth] -> [[Inline]] -> Block
table c hs as ws rs = Table nullAttr (caption c) (zip as ws) (thead as hs) (zipWith tbody as rs) nullTfoot

caption :: Text -> Caption
caption t = Caption Nothing [Plain [Str t]]

cell :: Alignment -> Inline -> Cell
cell a i = Cell nullAttr a (RowSpan 1) (ColSpan 1) [Plain [i]]

thead :: [Alignment] -> [Inline] -> TableHead
thead as xs = TableHead nullAttr (zipWith (\a i -> Row nullAttr [cell a i]) as xs)

nullTfoot :: TableFoot
nullTfoot = TableFoot nullAttr []

tbody :: Alignment -> [Inline] -> TableBody
tbody a xs = TableBody nullAttr (RowHeadColumns 0) [] (Row nullAttr . (:[]) . (cell a) <$> xs)


-- | code identifier classes text
-- >>> code "name" ["sourceCode", "literate", "haskell"] "x = 1\n"
-- CodeBlock ("name",["sourceCode","literate","haskell"],[]) "x = 1\n"
code :: Text -> [Text] -> Text -> Block
code name classes =
  CodeBlock (name, classes, [])

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
  runIO $ readMarkdown (def :: ReaderOptions) {readerExtensions = exts f} t

-- | render a pandoc AST
-- >>>
-- >>> renderMarkdown GitHubMarkdown (Pandoc mempty [table1])
-- Right "| first column  |     |\n|:--------------|----:|\n| second column |     |\n| first row     |     |\n| 1             |     |\n| second row    |     |\n| 1000          |     |\n"
renderMarkdown :: Flavour -> Pandoc -> Either PandocError Text
renderMarkdown f (Pandoc meta bs) =
  runPure $
    writeMarkdown
      (def :: WriterOptions) {writerExtensions = exts f}
      (Pandoc meta (tweakHaskellCodeBlock <$> bs))

-- | render a pandoc AST to Html
-- Note that text align for a Table cannot be blank when rendering to html
-- >>> Blaze.renderHtml <$> (runPure $ writeHtml5 (def {writerExtensions = (getDefaultExtensions "html")}) (Pandoc mempty [table1]))
-- Right "<table>\n<caption>an example table</caption>\n<thead>\n<tr class=\"header\">\n<th style=\"text-align: left;\">first column</th>\n<th style=\"text-align: right;\"></th>\n</tr>\n</thead>\n<tbody>\n<tr class=\"odd\">\n<td style=\"text-align: left;\">second column</td>\n<td style=\"text-align: right;\"></td>\n</tr>\n<tr class=\"even\">\n<td style=\"text-align: left;\">first row</td>\n<td style=\"text-align: right;\"></td>\n</tr>\n<tr class=\"odd\">\n<td style=\"text-align: left;\">1</td>\n<td style=\"text-align: right;\"></td>\n</tr>\n<tr class=\"even\">\n<td style=\"text-align: left;\">second row</td>\n<td style=\"text-align: right;\"></td>\n</tr>\n<tr class=\"odd\">\n<td style=\"text-align: left;\">1000</td>\n<td style=\"text-align: right;\"></td>\n</tr>\n</tbody>\n</table>"
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
                    Fence t -> [CodeBlock (id', classes, kv) t]
                    Replace t -> [plain t]
                    Native bs -> bs
                    RawHtml h -> [RawBlock (Format "html") h]
                )
                (Map.lookup x m)
          )
          (headMay . filter (`elem` classes) . Map.keys $ m)
      )
      ("output" `elem` classes)
  b' -> [b']

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
  either (pure . Left) (\t -> writeFile fo t >> pure (Right ())) w

