{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

import NumHask.Prelude
import Readme.Lhs

main :: IO ()
main =
  void $
  runOutput ("other/readme_.md", GitHubMarkdown) ("readme.md", GitHubMarkdown) $
  output "example" (Fence "Simple example of an output")
