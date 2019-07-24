{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Protolude
import Test.DocTest
import Readme.Lhs

-- | doctest
-- >>> :set -XOverloadedStrings

-- | The main differences between LHS and GitHubMarkdown is that GitHubMarkdown parses bird tracks as a BlockQuote.
-- >>> readPandoc "test/test.md" GitHubMarkdown
-- Right (Pandoc (Meta {unMeta = fromList []}) [Para [Str "haskell",Space,Str "LHS",Space,Str "style"],CodeBlock ("",["sourceCode","literate","haskell"],[]) "",Para [Str "bird-tracks"],BlockQuote [Para [Str "import",Space,Str "Readme.Lhs"]],Para [Str "code",Space,Str "block"],CodeBlock ("",[],[]) "indented\nunfenced code",Para [Str "github-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["haskell"],[]) "",Para [Code ("",[],[]) "output test1"],Para [Str "php-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["output","test1"],[]) ""])
-- >>> readPandoc "test/test.md" LHS
-- Right (Pandoc (Meta {unMeta = fromList []}) [Para [Str "haskell",Space,Str "LHS",Space,Str "style"],CodeBlock ("",["sourceCode","literate","haskell"],[]) "",Para [Str "bird-tracks"],CodeBlock ("",["sourceCode","literate","haskell"],[]) "import Readme.Lhs",Para [Str "code",Space,Str "block"],CodeBlock ("",[],[]) "indented\nunfenced code",Para [Str "github-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["haskell"],[]) "",Para [Code ("",[],[]) "output test1"],Para [Str "php-style",Space,Str "fenced",Space,Str "code",Space,Str "blocks"],CodeBlock ("",["output","test1"],[]) ""])

main :: IO ()
main =
  doctest
  [ "src/Readme/Lhs.hs"
  , "test/test.hs"]

