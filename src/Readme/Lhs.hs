{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Readme.Lhs
  ( -- * restricted pandoc types
    Flavour (..),
    readPandoc,
    renderPandoc,

    -- * output
    Output (..),
    OutputMap,
    output,
    insertOutput,
    runOutput,
    tweakHaskellCodeBlock,

    -- * common patterns
    defaultTable,
    defaultTextTable,
    bootTableAttr,
    thead,
    tbody,
    cell1,
    badge,
    hask,

    -- * exports
    module Text.Pandoc.Definition,
    module B,
  )
where

import qualified Data.Map as Map
import NumHask.Prelude hiding (link)
import qualified Text.Blaze.Html.Renderer.Text as Blaze
import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Builder as B

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Readme.Lhs
-- >>> import Text.Pandoc.Builder as B
-- >>> let table1 = Table ("",["table","table-bordered","table-hover","m-3"],[("style","width: 70%;")]) (Caption Nothing [Plain [Str "an",Space,Str "example",Space,Str "table"]]) [(AlignLeft,ColWidthDefault),(AlignRight,ColWidthDefault)] (TableHead ("",[],[]) [Row ("",[],[]) [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "first",Space,Str "column"]],Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "second",Space,Str "column"]]]]) [TableBody ("",[],[]) (RowHeadColumns 0) [] [Row ("",[],[]) [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "first",Space,Str "row"]],Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "1"]]],Row ("",[],[]) [Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "second",Space,Str "row"]],Cell ("",[],[]) AlignDefault (RowSpan 1) (ColSpan 1) [Plain [Str "1000"]]]]] (TableFoot ("",[],[]) [])

-- | use LHS when you want to just add output to a *.lhs
--
--   use GitHubMarkdown for rendering code and results on github
--
--   The main differences between LHS and GitHubMarkdown is that GitHubMarkdown parses bird tracks as a BlockQuote.
-- >>> readPandoc "test/test.md" GitHubMarkdown
-- Right (Pandoc (Meta {unMeta = fromList []}) [Para [Str "haskell",Space,Str "LHS",Space,Str "style"],CodeBlock ("",["sourceCode","literate","haskell"],[]) "",Para [Str "bird-tracks"],BlockQuote [Para [Str "import",Space,Str "Readme.Lhs"]],Para [Str "code",Space,Str "block"],CodeBlock ("",[],[]) "indented\nunfenced code",Para [Str "github-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["haskell"],[]) "",Para [Code ("",[],[]) "output test1"],Para [Str "php-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["output","test1"],[]) "",Para [Str "raw",Space,Str "html"],RawBlock (Format "html") "<div><br><p>I am raw Html</p></div>"])
--
-- >>> readPandoc "test/test.md" LHS
-- Right (Pandoc (Meta {unMeta = fromList []}) [Plain [Str "haskell",Space,Str "LHS",Space,Str "style",SoftBreak,Str "```{.sourceCode",Space,Str ".literate",Space,Str ".haskell}",SoftBreak,Str "```",SoftBreak,Str "bird-tracks",SoftBreak,Str ">",Space,Str "import",Space,Str "Readme.Lhs",SoftBreak,Str "code",Space,Str "block",SoftBreak,Str "indented",SoftBreak,Str "unfenced",Space,Str "code",SoftBreak,Str "github-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks",SoftBreak,Str "```",Space,Str "haskell",SoftBreak,Str "```",SoftBreak,Str "```",Space,Str "output",Space,Str "test1",SoftBreak,Str "```",SoftBreak,Str "php-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks",SoftBreak,Str "```",Space,Str "{.output",Space,Str ".test1}",SoftBreak,Str "```",SoftBreak,Str "raw",Space,Str "html"],Div ("",[],[]) [Plain [LineBreak],Para [Str "I",Space,Str "am",Space,Str "raw",Space,Str "Html"]]])
--
-- Note how raw html inside markdown files is broken.
--
-- >>> (Right (Pandoc _ t1)) <- readPandoc "test/table1.html" Html
-- >>> t1 == [table1]
-- True
data Flavour = GitHubMarkdown | LHS | Html deriving (Eq, Show, Ord)

-- | exts LHS is equivalent to 'markdown+lhs'
-- exts GitHubMarkdown is equivalent to 'gfm'
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
readPandoc fp f
  | f == GitHubMarkdown = do
    t <- liftIO $ readFile fp
    runIO $ readMarkdown (def :: ReaderOptions) {readerExtensions = exts f} t
  | otherwise = do
    t <- liftIO $ readFile fp
    runIO $ readHtml (def :: ReaderOptions) {readerExtensions = exts f} t

-- | render a pandoc AST
--
-- >>> renderPandoc GitHubMarkdown (Pandoc mempty [table1])
-- Right "| first column | second column |\n|:-------------|--------------:|\n| first row    |             1 |\n| second row   |          1000 |\n"
--
-- >>> renderPandoc Html (Pandoc mempty [table1])
-- Right "<table class=\"table table-bordered table-hover m-3\" style=\"width: 70%;\">\n<caption>an example table</caption>\n<thead>\n<tr class=\"header\">\n<th style=\"text-align: left;\">first column</th>\n<th style=\"text-align: right;\">second column</th>\n</tr>\n</thead>\n<tbody>\n<tr class=\"odd\">\n<td style=\"text-align: left;\">first row</td>\n<td style=\"text-align: right;\">1</td>\n</tr>\n<tr class=\"even\">\n<td style=\"text-align: left;\">second row</td>\n<td style=\"text-align: right;\">1000</td>\n</tr>\n</tbody>\n</table>"
--
-- Note how pandoc strips things like links, style and scripts.
--
renderPandoc :: Flavour -> Pandoc -> Either PandocError Text
renderPandoc f (Pandoc meta bs)
  | f == Html = runPure $ do
    h <-
      writeHtml5
        (def :: WriterOptions) {writerExtensions = exts f}
        (Pandoc meta (tweakHaskellCodeBlock <$> bs))
    pure $ toStrict $ Blaze.renderHtml h
  | otherwise =
    runPure $
      writeMarkdown
        (def :: WriterOptions) {writerExtensions = exts f}
        (Pandoc meta (tweakHaskellCodeBlock <$> bs))

-- | output can be native pandoc, text that replaces or inserts into the output code block, or Html.
data Output = Native [Block] | Replace Text | Fence Text | RawHtml Text

type OutputMap = Map Text Output

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
                    Replace t -> (B.toList $ plain (str t))
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
        renderPandoc flavo p'
  either (pure . Left) (\t -> writeFile fo t >> pure (Right ())) w

-- | create a simple table from Inlines
-- >>> defaultTable bootTableAttr (B.fromList [Str "an",Space,Str "example",Space,Str "table"]) [(AlignLeft, ColWidthDefault), (AlignRight, ColWidthDefault)] (B.fromList <$> [[Str "first",Space,Str "column"], [Str "second",Space,Str "column"]]) (fmap B.fromList <$> [[[Str "first",Space,Str "row"], [Str "1"]], [[Str "second",Space,Str "row"], [Str "1000"]]]) == singleton table1
-- True
defaultTable :: Attr -> Inlines -> [ColSpec] -> [Inlines] -> [[Inlines]] -> Blocks
defaultTable attr c cs hs rs = tableWith attr (simpleCaption (plain c)) cs (thead hs) ((:[]) $ tbody rs) nullTfoot

-- | create a simple table from Text
--
-- > defaultTextTable bootTableAttr "an example table" [(AlignLeft, ColWidthDefault), (AlignRight, ColWidthDefault)] ["first column", "second column"] [["first row", "1"], ["second row", "1000"]]
defaultTextTable :: Attr -> Text -> [ColSpec] -> [Text] -> [[Text]] -> Blocks
defaultTextTable attr c cs hs rs = defaultTable attr (str c) cs (str <$> hs) (fmap str <$> rs)

-- | bootstrap classes
bootTableAttr :: Attr
bootTableAttr = ("",["table","table-bordered","table-hover","m-3"],[("style","width: 70%;")])

-- | aligned simple cell
cell1 :: Alignment -> Inlines -> Cell
cell1 a i = cell a (RowSpan 1) (ColSpan 1) (plain i)

-- | aligned simple table header
thead :: [Inlines] -> TableHead
thead xs = TableHead nullAttr . (:[]) . Row nullAttr $ (simpleCell . plain <$> xs)

-- | null table footer
nullTfoot :: TableFoot
nullTfoot = TableFoot nullAttr []

-- | aligned simple table body
tbody :: [[Inlines]] -> TableBody
tbody xs = TableBody nullAttr (RowHeadColumns 0) [] (Row nullAttr <$> (fmap (simpleCell . plain) <$> xs))

-- | haskell code block
hask :: Maybe Text -> Text -> Inlines
hask name t = codeWith (fromMaybe mempty name, ["sourceCode","literate","haskell"], []) t

-- | create a badge link
-- >>> B.toList $ badge "Build Status" "https://travis-ci.org/tonyday567/readme-lhs.svg" "https://travis-ci.org/tonyday567/readme-lhs"
-- [Link ("",[],[]) [Image ("",[],[]) [Str "Build Status"] ("https://travis-ci.org/tonyday567/readme-lhs.svg","")] ("https://travis-ci.org/tonyday567/readme-lhs","")]
badge :: Text -> Text -> Text -> Inlines
badge label badge' url =
  singleton $
  Link ("", [], [])
  [Image ("", [], []) [Str label] (badge', "")]
  (url, "")
