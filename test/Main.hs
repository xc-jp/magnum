{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Numeric.Magnum
import Control.Monad

prop_showReadShow :: Int -> Maybe Int -> Bool
prop_showReadShow n digits = 
  let str = showNum digits n 
      Just (num :: Int) = readNum str
      str' = showNum digits num
   in str == str'

return []

main :: IO ()
main = void $quickCheckAll
