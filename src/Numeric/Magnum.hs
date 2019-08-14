module Numeric.Magnum
  ( readNum
  , showNum
  ) where

import           Data.Char (isDigit, toUpper)
import           Text.Read (readMaybe)

-- | Read a number with a postfix, e.g
--
-- >>> (readNum "9M" :: Int)
-- Just 9000000
--
readNum :: Num a => String -> Maybe a
readNum str = do
    let (base', expo') = span isDigit str
    base <- readMaybe base'
    expo <- readExp . fmap toUpper . dropWhile (==' ') $ expo'
    pure $ fromInteger base * expo
  where
    readExp ""  = Just 1
    readExp "K" = Just 1000
    readExp "M" = Just 1000000
    readExp "G" = Just 1000000000
    readExp "T" = Just 1000000000000
    readExp "P" = Just 1000000000000000
    readExp "E" = Just 1000000000000000000
    readExp "Z" = Just 1000000000000000000000
    readExp "Y" = Just 1000000000000000000000000
    readExp _   = Nothing

{-# INLINE atExp #-}
atExp :: Integral a => a -> a -> Maybe a
atExp n e = if n' > 0 then Just n' else Nothing
  where
    e' = 10^e
    n' = n `div` e'

-- | Show a number with a postfix:
--
-- >>> showNum 99000000
-- "99M"
--
showNum :: (Show a, Integral a) => a -> String
showNum n
  | Just n' <- atExp n 24 = show n' <> "Y"
  | Just n' <- atExp n 21 = show n' <> "Z"
  | Just n' <- atExp n 18 = show n' <> "E"
  | Just n' <- atExp n 15 = show n' <> "P"
  | Just n' <- atExp n 12 = show n' <> "T"
  | Just n' <- atExp n 9  = show n' <> "G"
  | Just n' <- atExp n 6  = show n' <> "M"
  | Just n' <- atExp n 3  = show n' <> "K"
  | otherwise = show n
