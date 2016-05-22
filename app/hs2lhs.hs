{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-
 Comments starting with CurlDashPipe or CurlDashStar are considered to be part of code
-}

-- Comments after `--` are comments (if starting a new line) and comments after `-- |`
-- | are considered to be code

module Main where

import            Control.Applicative
import            Data.Maybe          (fromMaybe)
import            Data.Text           (Text, stripStart, stripPrefix, isPrefixOf, isSuffixOf)
import qualified  Data.Text           as T
import qualified  Data.Text.IO        as T
-- import            System.Environment
import qualified System.IO as IO
import Control.Lens
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad
import Data.Monoid
import Control.Monad.Morph

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

lhsLine :: Config -> Text -> S.State S Text
lhsLine c t = do
    prev <- use sPrevious
    when isBlank (sBlankCount += 1)
    b <- use sBlankCount
    inCdp <- use sInCurlDashPipe
    inCd <- use sInCurlDash
    if
        | isBlank && 
          (prev==Comment ||
           (b > (c ^. nBlanksIsContinue) 
            && prev==Code)) -> do
              sPrevious .= Comment
              return t
        | stripIf "-- |" ddpIsCode -> commentStrip "-- |"
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
        | stripIf "-}" cdpIsCode -> do
              sInCurlDash .= False
              sInCurlDashPipe .= False
              commentStrip "-}"
        | inCdp || inCd && not (c ^. cdIsCode) -> commentStrip ""
        | otherwise -> codeDecorate "> "
  where
    isBlank = stripStart t == T.empty
    stripIf p c' = isPrefixOf p t && not (c ^. c')
    commentStrip p = do
              sPrevious .= Comment
              return $ fromMaybe t (stripPrefix p t)
    codeDecorate p = do
              sPrevious .= Code
              return $ p <> t

lhsLine' :: Config -> Text -> S.StateT S IO Text
lhsLine' c t = do
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
              commentStrip "" "isBlank"
        | stripIf "-- |" ddpIsCode ->
              commentStrip "-- |" "-- |"
        | stripIf "-- *" ddpIsCode -> commentStrip "-- *" "-- *"
        | ("--" `isPrefixOf` t) && 
          not ("-- |" `isPrefixOf` t) &&
          not ("-- *" `isPrefixOf` t) &&
          not (c ^. ddIsCode) -> commentStrip "--" "--"
        | isPrefixOf "{-#" t && isSuffixOf "#-}" t && not (c ^. cdhIsCode) ->
              commentStrip "" "pragma"
        | stripIf "{- |" cdpIsCode -> 
              sInCurlDashPipe .= True >> commentStrip "{- |" "{- |"
        | stripIf "{- *" cdpIsCode -> 
              sInCurlDashPipe .= True >> commentStrip "{- *" "{- *"
        | ("{-" `isPrefixOf` t) && 
          not ("{- |" `isPrefixOf` t) && 
          not ("{- *" `isPrefixOf` t) &&
          not ("{-#"  `isPrefixOf` t) &&
          not (c ^. cdIsCode) -> sInCurlDash .= True >> commentStrip "{-" "{-"
        | stripIf "| -}" cdpIsCode -> 
              sInCurlDashPipe .= False >> commentStrip "| -}" "| -}"
        | stripIf "* -}" cdpIsCode -> 
              sInCurlDashPipe .= False >> commentStrip "* -}" "* -}"
        | stripIf "-}" cdIsCode -> do
              sInCurlDash .= False
              sInCurlDashPipe .= False
              commentStrip "-}" "-}"
        | inCdp || inCd && not (c ^. cdIsCode) ->
              commentStrip "" "inCD"
        | otherwise ->
              codeDecorate "> " "code"
  where
    isBlank = stripStart t == T.empty
    stripIf p c' = isPrefixOf p t && not (c ^. c')
    commentStrip p pr = do
              sPrevious .= Comment
              lift $ print pr
              return $ fromMaybe t (stripPrefix p t)
    codeDecorate p pr = do
              sPrevious .= Code
              lift $ print pr
              return $ p <> t

 
lhs :: [Text] -> IO [Text]
lhs ts = (`S.evalStateT` initialState)
         (sequence $ lhsLine' defaultConfig <$> ts)

{-
main' :: IO ()
main' = do
  text <- T.readFile . head =<< getArgs
  let p = T.lines text
  mapM_ T.putStrLn (lhs (Code,0) p)
-}

main :: IO ()
main = do
  text <- T.readFile "hs2lhs.hs"
  let p = T.lines text
  IO.withFile "hs2lhs.lhs" IO.WriteMode $ \h -> do
      p' <- lhs p
      mapM_ (T.hPutStrLn h) p'

data HS = HS { _hsCommentBlock :: [Text]
             } deriving (Eq, Show)

makeLenses ''HS

initialHState :: HS
initialHState = HS []

hsLine' :: Text -> S.StateT HS IO [Text]
hsLine' t =
    if  | ("> " `isPrefixOf` t) -> do
              priorComments <- use hsCommentBlock
              hsCommentBlock .= []
              return $ pad priorComments ++ [fromMaybe t (stripPrefix "> " t)]
        | otherwise -> do
              hsCommentBlock %= (++ [t])
              return mempty

pad :: [Text] -> [Text]
pad cs = case length cs of
              0 -> mempty
              1 -> ["-- " <> head cs]
              _ -> ["{-"] ++ cs ++ ["-}"]  

hs :: [Text] -> IO [Text]
hs ts = do
    (ans,st) <- (`S.runStateT` initialHState)
         (sequence $ hsLine' <$> ts)
    return $ concat ans ++ pad (st ^. hsCommentBlock)

lhs2hs :: IO ()
lhs2hs = do
  text <- T.readFile "example.lhs"
  let p = T.lines text
  IO.withFile "example.hs" IO.WriteMode $ \h -> do
      p' <- hs p
      mapM_ (T.hPutStrLn h) p'



{-

λ> (`S.runState` initialState) (sequence $ lhsLine defaultConfig <$> ["",""])
(["> ",""],S {_sPrevious = Comment, _sBlankCount = 2, _sInCurlDash = False, _sInCurlDashPipe = False})
λ> 

λ> (`S.runState` initialState) (sequence $ lhsLine defaultConfig <$> ["{-# pragma #-}","code","","","-- a normal comment","","-- | a special comment","","","-- * another one"])
(["{-# pragma #-}","> code","> ","","> -- a normal comment","","> -- | a special comment","","","> -- * another one"],S {_sPrevious = Code, _sBlankCount = 5, _sInCurlDash = False, _sInCurlDashPipe = False})

λ> (`S.runStateT` initialState) (sequence $ lhsLine' defaultConfig <$> ["{-# pragma #-}","code","","","-- a normal comment","","-- | a special comment","","","-- * another one"])
"code line!"
"code line!"
"code line!"
"isBlank"
"--"
"isBlank"
"code line!"
"isBlank"
"isBlank"
"code line!"
(["> {-# pragma #-}","> code","> ",""," a normal comment","","> -- | a special comment","","","> -- * another one"],S {_sPrevious = Code, _sBlankCount = 5, _sInCurlDash = False, _sInCurlDashPipe = False})


-}

