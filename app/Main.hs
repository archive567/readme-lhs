{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Applicative
import Protolude
import Readme.Lhs
import System.FilePath

data Options = Options
    { fileIn :: FilePath
    , fileOut :: FilePath
    } deriving (Show, Eq)

options :: Options.Applicative.Parser Options
options = Options
    <$> (strOption
        (long "in" <> short 'i' <> value "" <> help "file in"))
    <*> (strOption
        (long "out" <> short 'o' <> value "" <> help "file out"))

consume :: Options -> IO ()
consume (Options "" _) = pure ()
consume (Options fi fo) = do
    text <- readFile fi
    let fromHs = ".hs" == takeExtension fi
    let fo' = dropExtension fi ++ (if fromHs then ".lhs" else ".hs")
    withFile (if fo == "" then fo' else fo) WriteMode $ \h -> do
        let f = parse (if fromHs then Hs else Lhs) (Text.lines text)
        mapM_ (Text.hPutStrLn h) (Readme.Lhs.print (if fromHs then Lhs else Hs) f)

main :: IO ()
main = execParser opts >>= consume
  where
    opts = info (helper <*> options)
      ( fullDesc
      <> progDesc "almost isomorph of .lhs and .hs formats"
      <> header "lhs")
