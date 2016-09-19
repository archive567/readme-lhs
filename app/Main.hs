{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Protolude hiding ((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative
import Control.Lens
import System.FilePath
import Readme.Lhs

config :: Parser Config
config = Config
    <$> (strOption
          (long "file" <>
            value "" <>
            help "file to convert"))
    <*> (option auto
          (long "trailing" <>
            value 1 <>
            help "number of trailing blank lines considered to still be code"))
    <*> (option auto
          (long "section" <>
            value Code <>
            help "which section assumed at start of parsing"))

consume :: Config -> IO ()
consume (Config "" _ _) = pure ()
consume cfg@(Config file _ _) = do
  text <- readFile file
  let isHs = ".hs" == takeExtension file
  let fo = dropExtension file ++ (if isHs then ".lhs" else ".hs")
  withFile fo WriteMode $ \h -> do
      let p' = (if isHs then lhs else hs) cfg (Text.lines text)
      mapM_ (Text.hPutStrLn h) p'

main :: IO ()
main = execParser opts >>= consume
  where
    opts = info (helper <*> config)
        ( fullDesc
        <> progDesc "convert between lhs and hs"
        <> header "lhs")

