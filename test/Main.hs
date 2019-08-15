{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Numeric.Magnum
import Control.Monad

prop_readShow :: Int -> Maybe Int -> Bool
prop_readShow n digits = 
  let str = showNum digits n 
      Just (num :: Int) = readNum str
      str' = showNum digits num
   in str == str'

prop_showRead :: Int -> Bool
prop_showRead n =
  let str = showNum (Just maxBound) n
      Just num = readNum str
   in n == num

return []

main :: IO ()
main = void $quickCheckAll
