{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.Magnum
  ( readNum
  , showNum
  ) where

import           Data.Char (isDigit, toUpper)

-- | Read a number with a postfix, e.g
--
-- >>> (readNum "9M" :: Int)
-- Just 9000000
--
readNum :: Num a => String -> Maybe a
readNum str' = do
    let (minus, str) = case str' of
                        '-' : t -> (True, t)
                        t -> (False, t)
    let (base', expo') = span isNumChar str
    expo <- readExp . fmap toUpper . dropWhile (==' ') $ expo'
    let (units,decimals') = span isDigit base'
    decimals <- case decimals' of
      "" -> Just ""
      '.' : ds | all isDigit ds -> Just ds
      _ -> Nothing
    let n = fromInteger $ read $ units <> take expo (decimals <> repeat '0')
    pure $ if minus then negate n else n
  where
    isNumChar a = isDigit a || (a == '.')
    readExp ""  = Just 0
    readExp "K" = Just 3
    readExp "M" = Just 6
    readExp "G" = Just 9
    readExp "T" = Just 12
    readExp "P" = Just 15
    readExp "E" = Just 18
    readExp "Z" = Just 21
    readExp "Y" = Just 24
    readExp _   = Nothing

-- | Show a number with a postfix:
--
-- >>> showNum (Just 3) 99000000
-- "99.000M"
--
showNum :: (Show a, Integral a) => Maybe Int -> a -> String
showNum digits n = if n < 0 then '-' : go (negate n) else go n
  where
    go n | Just n' <- atExp n 24 = show' n' <> "Y"
         | Just n' <- atExp n 21 = show' n' <> "Z"
         | Just n' <- atExp n 18 = show' n' <> "E"
         | Just n' <- atExp n 15 = show' n' <> "P"
         | Just n' <- atExp n 12 = show' n' <> "T"
         | Just n' <- atExp n 9  = show' n' <> "G"
         | Just n' <- atExp n 6  = show' n' <> "M"
         | Just n' <- atExp n 3  = show' n' <> "K"
         | otherwise = show n
    atExp :: Integral a => a -> a -> Maybe (a,a)
    atExp n e = if n' /= 0 then Just (n',r') else Nothing
      where
        e' = 10^e
        (n',r') = divMod n e'
    show' (b,e) =
      case digits of
        Just r | r > 0 -> show b <> "." <> take r (show e)
        _ -> show b
