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
import qualified Control.Foldl as L
import Readme.Lhs

data Options = Options
    { fileIn :: FilePath
    , fileOut :: FilePath
    , trailingBlanks :: Int
    } deriving (Show, Eq)

options :: Parser Options
options = Options
    <$> (strOption
        (long "in" <> short 'i' <> value "" <> help "file in"))
    <*> (strOption
        (long "out" <> short 'o' <> value "" <> help "file out"))
    <*> (option auto (long "trailing" <> value 1 <> help "number of trailing blanks considered to be code"))

consume :: Options -> IO ()
consume (Options "" _ _) = pure ()
consume (Options fi fo tb) = do
  text <- readFile fi
  let isHs = ".hs" == takeExtension fi
  let fo' = dropExtension fi ++ (if isHs then ".lhs" else ".hs")
  withFile (if fo == "" then fo' else fo) WriteMode $ \h -> do
      let p' = (if isHs then lhs else hs) (Config tb Code) (Text.lines text)
      mapM_ (Text.hPutStrLn h) p'

main :: IO ()
main = execParser opts >>= consume
  where
    opts = info (helper <*> options)
        ( fullDesc
        <> progDesc "convert between lhs and hs"
        <> header "lhs")

