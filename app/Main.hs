{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Options.Generic
import Protolude
import Readme.Lhs
import System.FilePath

data Options = Options
    { filein :: FilePath <?> "file in"
    , fileout :: FilePath <?> "file out"
    } deriving (Generic, Show)

instance ParseRecord Options

consume :: Options -> IO ()
consume (Options (Helpful "") _) = pure ()
consume (Options (Helpful fi) (Helpful fo)) = do
    text <- readFile fi
    let fromHs = ".hs" == takeExtension fi
    let fo' = dropExtension fi ++ (if fromHs then ".lhs" else ".hs")
    withFile (if fo == "" then fo' else fo) WriteMode $ \h -> do
        let f = parse (if fromHs then Hs else Lhs) (Text.lines text)
        mapM_ (Text.hPutStrLn h) (Readme.Lhs.print (if fromHs then Lhs else Hs) f)

main :: IO ()
main = consume =<< getRecord "lhs"
