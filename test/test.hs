{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Main where

import NumHask.Prelude
import Test.DocTest
import Readme.Lhs

main :: IO ()
main =
  doctest
  [ "src/Readme/Lhs.hs"
  ]
