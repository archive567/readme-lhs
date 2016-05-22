{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-
 Comments starting with CurlDashPipe or CurlDashStar are considered to be part of code
-}

-- Comments after `--` are comments (if starting a new line) and comments after `-- |`
-- | are considered to be code

{-

hs and lhs should be pretty isomorphic

hs . lhs $ "> import Control.Monad\n\n> x = 1 --comment\n\nStart of comment\na markdown header\n---\n\n> main = print show x"
-}

module Main where

import            Options.Applicative
import            Data.Maybe          (fromMaybe)
import            Data.Text           (Text, stripStart, stripPrefix, isPrefixOf, isSuffixOf)
import qualified  Data.Text           as Text
import qualified  Data.Text.IO           as Text
import Control.Lens hiding (argument)
import qualified Control.Monad.Trans.State.Strict as S
import qualified System.IO as IO
import qualified System.FilePath as FilePath
import Control.Monad (join)

data Config = Config
    { _nBlanksIsContinue :: Int
    , _ddIsCode :: Bool
    , _ddpIsCode :: Bool
    , _cdhIsCode :: Bool
    , _cdIsCode :: Bool
    , _cdpIsCode :: Bool
    }

makeLenses ''Config

defaultConfig :: Config
defaultConfig = Config 1 False True True False True

data Tag = Comment | Code deriving (Show, Eq)

data S = S { _sPrevious :: Tag
           , _sBlankCount :: Int
           , _sInCurlDash :: Bool
           , _sInCurlDashPipe :: Bool
           } deriving (Eq, Show)

makeLenses ''S

initialState :: S
initialState = S Code 0 False False

lhs2hs :: Config -> Text -> S.State S Text
lhs2hs c t = do
    prev <- use sPrevious
    sBlankCount %= if isBlank then (+1) else const 0 
    b <- use sBlankCount
    inCdp <- use sInCurlDashPipe
    inCd <- use sInCurlDash
    if
        | isBlank && 
          (prev==Comment ||
           (b > (c ^. nBlanksIsContinue) 
            && prev==Code)) ->
              commentStrip ""
        | stripIf "-- |" ddpIsCode ->
              commentStrip "-- |"
        | stripIf "-- *" ddpIsCode -> commentStrip "-- *"
        | ("--" `isPrefixOf` t) && 
          not ("-- |" `isPrefixOf` t) &&
          not ("-- *" `isPrefixOf` t) &&
          not (c ^. ddIsCode) -> commentStrip "--"
        | isPrefixOf "{-#" t && isSuffixOf "#-}" t && not (c ^. cdhIsCode) ->
              commentStrip ""
        | stripIf "{- |" cdpIsCode -> 
              sInCurlDashPipe .= True >> commentStrip "{- |"
        | stripIf "{- *" cdpIsCode -> 
              sInCurlDashPipe .= True >> commentStrip "{- *"
        | ("{-" `isPrefixOf` t) && 
          not ("{- |" `isPrefixOf` t) && 
          not ("{- *" `isPrefixOf` t) &&
          not ("{-#"  `isPrefixOf` t) &&
          not (c ^. cdIsCode) -> sInCurlDash .= True >> commentStrip "{-"
        | stripIf "| -}" cdpIsCode -> 
              sInCurlDashPipe .= False >> commentStrip "| -}"
        | stripIf "* -}" cdpIsCode -> 
              sInCurlDashPipe .= False >> commentStrip "* -}"
        | stripIf "-}" cdIsCode -> do
              sInCurlDash .= False
              sInCurlDashPipe .= False
              commentStrip "-}"
        | inCdp || inCd && not (c ^. cdIsCode) ->
              commentStrip ""
        | otherwise ->
              codeDecorate "> "
  where
    isBlank = stripStart t == Text.empty
    stripIf p c' = isPrefixOf p t && not (c ^. c')
    commentStrip p = do
              sPrevious .= Comment
              return $ fromMaybe t (stripPrefix p t)
    codeDecorate p = do
              sPrevious .= Code
              return $ p <> t

 
hs :: Text -> Text
hs ts = runIdentity $ (`S.evalStateT` initialState) (lhs2hs defaultConfig ts)

data S' = S' { _hsCommentBlock :: [Text]
             } deriving (Eq, Show)

makeLenses ''S'

initialHState :: S'
initialHState = S' []

hs2lhs :: Text -> S.State S' Text
hs2lhs t =
    if  | ("> " `isPrefixOf` t) -> do
              priorComments <- use hsCommentBlock
              hsCommentBlock .= []
              pure $ mconcat $ pad priorComments ++ [fromMaybe t (stripPrefix "> " t)]
        | otherwise -> do
              hsCommentBlock %= (++ [t])
              return mempty

pad :: [Text] -> [Text]
pad cs = case length cs of
              0 -> mempty
              1 -> ["-- " <> head cs]
              _ -> ["{-"] ++ cs ++ ["-}"]  

lhs :: Text -> Text
lhs ts = runIdentity $ (`S.evalStateT` initialHState) (hs2lhs ts)

data HSType = LHS | HS

data Options = Options
               { inFile :: FilePath
               , outFile :: FilePath
               }

options :: Parser Options
options = Options
  <$> strOption
      ( long "in file"
     <> metavar "TARGET"
     <> help "file to convert" )
  <*> strOption
      ( long "quiet"
     <> help "Whether to be quiet" )

main' :: IO ()
main' = execParser opts >>= greet
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

greet :: Options -> IO ()
greet (Options fi fo) =
  case fi of
    "hs" -> IO.withFile fo IO.WriteMode $ \h -> fmap lhs (Text.readFile fi) >>= Text.hPutStr h
    x -> error x

data App = App { fileIn :: FilePath
               , fileOut :: FilePath
               }

runWithOptions :: App -> IO ()
runWithOptions (App fi "") =
    case FilePath.takeExtension fi of
        ".lhs" -> let fo = FilePath.dropExtension fi ++ ".hs" in
            join $ (Text.writeFile fo . hs) <$> Text.readFile fi
        ".hs"  -> let fo = FilePath.dropExtension fi ++ ".lhs" in
            join $ (Text.writeFile fo . lhs) <$> Text.readFile fi
runWithOptions (App fi fo) =
    case FilePath.takeExtension fi of
        ".lhs" -> join $ (Text.writeFile fo . hs) <$> Text.readFile fi
        ".hs"  -> join $ Text.writeFile fo <$> lhs <$> Text.readFile fi

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = App <$> argument str mempty
                 <*> argument str mempty
    opts = info parser mempty
