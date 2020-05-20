{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

import Protolude
import Readme.Lhs

main :: IO ()
main =
  void $
  runOutput ("other/readme_.md", GitHubMarkdown) ("readme.md", GitHubMarkdown) $
  output "example" (Fence "Simple example of an output")
