{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import Prelude
import Test.DocTest
import Readme.Lhs
import qualified Data.Map as Map

main :: IO ()
main =
  doctest
  [ "src/Readme/Lhs.hs",
    "src/Readme/Format.hs"
  ]
