module Readme.Lhs where

import Protolude
import qualified Control.Foldl as L
import qualified Data.Text as Text

data Section = Comment | Code deriving (Show, Eq)

data Config = Config
    { trailingBlanks :: Int
    , section :: Section
    } deriving (Show, Eq)

lhsLine trailing n s text = if
    | Text.stripStart text == "" && s == Code ->
          if trailing < n
          then ((n+1,Comment), [text])
          else ((n+1,Code), [bird text])
    | "{-#" `Text.isPrefixOf` text || "#-}" `Text.isSuffixOf` text ->
          ((0,Code), [bird text])
    | "{-" `Text.isPrefixOf` text && "-}" `Text.isSuffixOf` text ->
          ((0,Comment), [suffixStrip "-}" (prefixStrip "{-" text)])
    | "{-" == text ->
          ((0,Comment), [])
    | "{-" `Text.isPrefixOf` text ->
          ((0,Comment), [prefixStrip "{-" text])
    | "-}" == text ->
          ((0,Code), [])
    | "-}" `Text.isSuffixOf` text ->
          ((0,Code), [suffixStrip "-}" text])
    | s == Code -> ((0,Code), [bird text])
    | otherwise -> ((0,Comment), [text])
  where
    bird t = "> " <> t
    prefixStrip p t = fromMaybe t (Text.stripPrefix p t)
    suffixStrip p t = fromMaybe t (Text.stripSuffix p t)

lhs :: Config -> [Text] -> [Text]
lhs cfg@(Config trailing section) ts =
    L.fold (L.Fold step ((0,section), []) done) ts
  where
    done ((_,_), o) = o
    step ((n,s), o) a =
        let ((n',s'), a') = lhsLine trailing n s a
        in ((n',s'), o <> a')

hsLine s text = if
    | "> " `Text.isPrefixOf` text && s == Comment -> (Code, ["-}", unbird text])
    | "> " `Text.isPrefixOf` text -> (Code, [unbird text])
    | s == Code -> (Comment, ["{-", text])
    | otherwise -> (s, [text])
  where
    unbird t = fromMaybe t (Text.stripPrefix "> " t)

hs :: Config -> [Text] -> [Text]
hs (Config trailing section) ts = L.fold (L.Fold step (Code, []) done) ts
  where
    done (Comment,o) = o <> ["-}"]
    done (_,o) = o
    step (s, o) a =
        let (s', ts) = hsLine s a
        in (s', o <> ts)
