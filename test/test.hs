{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Prelude
import Test.DocTest
import Readme.Lhs
import qualified Data.Map as Map

-- | doctest
-- >>> :set -XOverloadedStrings
-- >>> import Readme.Lhs

-- | The main differences between LHS and GitHubMarkdown is that GitHubMarkdown parses bird tracks as a BlockQuote.
-- >>> readPandoc "test/test.md" GitHubMarkdown
-- Right (Pandoc (Meta {unMeta = fromList []}) [Para [Str "haskell",Space,Str "LHS",Space,Str "style"],CodeBlock ("",["sourceCode","literate","haskell"],[]) "",Para [Str "bird-tracks"],BlockQuote [Para [Str "import",Space,Str "Readme.Lhs"]],Para [Str "code",Space,Str "block"],CodeBlock ("",[],[]) "indented\nunfenced code",Para [Str "github-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["haskell"],[]) "",Para [Code ("",[],[]) "output test1"],Para [Str "php-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["output","test1"],[]) "",Para [Str "raw",Space,Str "html"],RawBlock (Format "html") "<div><br><p>I am raw Html</p></div>"])
-- >>> readPandoc "test/test.md" LHS
-- Right (Pandoc (Meta {unMeta = fromList []}) [Para [Str "haskell",Space,Str "LHS",Space,Str "style"],CodeBlock ("",["sourceCode","literate","haskell"],[]) "",Para [Str "bird-tracks"],CodeBlock ("",["haskell","literate"],[]) "import Readme.Lhs",Para [Str "code",Space,Str "block"],CodeBlock ("",[],[]) "indented\nunfenced code",Para [Str "github-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["haskell"],[]) "",Para [Code ("",[],[]) "output test1"],Para [Str "php-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["output","test1"],[]) "",Para [Str "raw",Space,Str "html"],Div ("",[],[]) [Plain [RawInline (Format "html") "<br>"],RawBlock (Format "html") "<p>",Plain [Str "I",Space,Str "am",Space,Str "raw",Space,Str "Html"],RawBlock (Format "html") "</p>"]])
--
-- >>> (Right (Pandoc _ bs)) <- readPandoc "test/test.md" GitHubMarkdown
-- >>> let p' = Pandoc mempty (mconcat $ insertOutput (Map.fromList [("test1", Replace "inserted text")]) <$> bs)
-- >>> renderHtml Html p'
-- Right "<p>haskell LHS style</p>\n<div class=\"sourceCode\" id=\"cb1\"><pre class=\"sourceCode haskell\"><code class=\"sourceCode haskell\"></code></pre></div>\n<p>bird-tracks</p>\n<blockquote>\n<p>import Readme.Lhs</p>\n</blockquote>\n<p>code block</p>\n<pre><code>indented\nunfenced code</code></pre>\n<p>github-style fenced code blocks</p>\n<div class=\"sourceCode\" id=\"cb3\"><pre class=\"sourceCode haskell\"><code class=\"sourceCode haskell\"></code></pre></div>\n<p><code>output test1</code></p>\n<p>php-style fenced code blocks</p>\ninserted text\n<p>raw html</p>\n<div><br><p>I am raw Html</p></div>"

main :: IO ()
main =
  doctest
  [ "src/Readme/Lhs.hs"
  , "test/test.hs"]

