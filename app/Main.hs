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

data Config = Config
    { file :: FilePath
    , codeBreak :: Int -- number of blanks lines that define a code/comment divide
    }

config :: Parser Config
config = Config
    <$> (strOption
          (long "file" <>
            value "default.lhs" <>
            help "file to convert"))
    <*> (option auto
          (long "break" <>
            value 1 <>
            help "number of lines defining a code/comment divide"))

data Tag = Comment | Code deriving (Show, Eq)

lhs :: Int -> [Text] -> [Text]
lhs break ts = L.fold (L.Fold step ((Code, 0), []) done) ts
  where
    done ((_,_), output) = output
    step :: ((Tag, Int), [Text]) -> Text -> ((Tag, Int), [Text])
    step ((tag, n), output) a =
        let ((tag', n'), a') = lhsLine (tag, n) a
        in ((tag', n'), output <> a')
    lhsLine (tag, n) text = do
        if
            | Text.stripStart text == "" && n < break -> ((tag, n+1), [text])
            | Text.stripStart text == "" -> ((toggle tag, 0), [text])
            | "---" `Text.isPrefixOf` text && tag == Comment -> ((Comment, 0), [text])
            | "--" `Text.isPrefixOf` text -> ((Code, 0), [bird text])
            | "{-#" `Text.isPrefixOf` text || "#-}" `Text.isSuffixOf` text ->
                  ((Code,0), [bird text])
            | "{-" == text -> ((Comment,0), [])
            | "-}" == text -> ((Code,0), [])
            | "{-" `Text.isPrefixOf` text -> ((Comment,0), [prefixStrip "{-" text])
            | "-}" `Text.isSuffixOf` text -> ((Code,0), [suffixStrip "-}" text])
            | tag == Code -> ((Code,0), [bird text])
            | otherwise -> ((tag,0), [text])
          where
            bird t = "> " <> t
            toggle Code = Comment
            toggle Comment = Code
            prefixStrip p t = fromMaybe t (Text.stripPrefix p t)
            suffixStrip p t = fromMaybe t (Text.stripSuffix p t)

hs :: [Text] -> [Text]
hs ts = L.fold (L.Fold step (Code, []) snd) ts
  where
    done (Code,output) = output
    done (Comment,output) = output <> ["-}"]
    step :: (Tag, [Text]) -> Text -> (Tag, [Text])
    step (section, output) a =
        let (tag, text) = lhsLine section a in (tag, output <> text)
    lhsLine tag text = do
        if
            | "> " `Text.isPrefixOf` text && tag == Comment -> (Code, ["-}", unbird text])
            | "> " `Text.isPrefixOf` text -> (Code, [unbird text])
            | tag == Code -> (Comment, ["{-", text])
            | otherwise -> (tag, [text])
          where
            unbird t = fromMaybe t (Text.stripPrefix "> " t)

consume :: Config -> IO ()
consume (Config "" _) = pure ()
consume (Config file break) = do
  text <- readFile file
  let isHs = ".hs" == takeExtension file
  let fo = dropExtension file ++ (if isHs then ".lhs" else ".hs")
  withFile fo WriteMode $ \h -> do
      let p' = (if isHs then lhs break else hs) (Text.lines text)
      mapM_ (Text.hPutStrLn h) p'

main :: IO ()
main = execParser opts >>= consume
  where
    opts = info (helper <*> config)
        ( fullDesc
        <> progDesc "convert between lhs and hs"
        <> header "lhs")

