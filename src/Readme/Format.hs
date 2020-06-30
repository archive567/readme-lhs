{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

-- | formatting of numbers and statistics
module Readme.Format
  ( average,
    deciles,
    percentile,
    fixed,
    commas,
    prec,
    secs,
  ) where

import NumHask.Prelude
import Data.Scientific
import Data.TDigest
import qualified Data.Text as Text

-- | average
--
-- >>> average [0..1000]
-- 500.0
average :: (Foldable f) => f Double -> Double
average xs = sum xs / fromIntegral (length xs)

-- | compute deciles
--
-- >>> deciles 5 [1..1000]
-- [1.0,200.5,400.5,600.5000000000001,800.5,1000.0]
--
deciles :: (Foldable f) => Int -> f Double -> [Double]
deciles n xs =
  ( \x ->
      fromMaybe 0 $
        quantile x (tdigest xs :: TDigest 25)
  )
    <$> ((/ fromIntegral n) . fromIntegral <$> [0 .. n])

-- | compute a percentile
--
-- >>> percentile 0.1 [1..1000]
-- 100.5
--
percentile :: (Foldable f) => Double -> f Double -> Double
percentile p xs = fromMaybe 0 $ quantile p (tdigest xs :: TDigest 25)

-- | fixed
-- >>> fixed 2 22.256
-- "22.26"
fixed :: Int -> Double -> Text
fixed n x
  | Text.length a == 0 = "0." <> (Text.replicate (n - Text.length b) "0") <> b
  | Text.length b == 0 = a
  | otherwise = a <> "." <> b
  where
    i = show (round (x * (10.0 ^ n)) :: Integer) :: Text
    (a, b) = Text.splitAt (Text.length i - n) i

-- | format a Float as a Scientific with a precision
-- >>> prec 3 123456
-- "1.235e5"
prec :: Int -> Double -> Text
prec p x = sciprec p (fromFloatDigits x)

-- | format with 1000 separated commas, and a fixed decimal part
-- >>> commas 2 123456.789
-- "123,456.79"
commas :: Int -> Double -> Text
commas n a
  | a < 0 = "-" <> commas n (- a)
  | a < 1000.0 = fixed n a
  | otherwise = intPart (floor a) "" <> decPart (a - fromInteger (floor a))
  where
    intPart :: Integer -> Text -> Text
    intPart x t
      | x < 1000 = Text.pack (show x) <> t
      | otherwise =
        let (d, m) = divMod x 1000
         in intPart d ("," <> Text.pack (show m))
    decPart :: Double -> Text
    decPart x = Text.drop 1 $ fixed n x

sciprec :: Int -> Scientific -> Text
sciprec n x = Text.pack $ formatScientific Exponent (Just n) x

-- | format as seconds
-- >>> secs 2 0.0000002345
-- "234ns"
secs :: Int -> Double -> Text
secs p s
  | s < 0.0 = "-" <> secs p (- s)
  | s >= 1.0 = fixed p s <> "s"
  | s >= 1e-3 = fixed 0 (s * 1e3) <> "ms"
  | s >= 1e-6 = fixed 0 (s * 1e6) <> "μs"
  | s >= 1e-9 = fixed 0 (s * 1e9) <> "ns"
  | otherwise = fixed p (s * 1e12) <> "ps"
