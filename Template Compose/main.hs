{-# LANGUAGE TemplateHaskell #-}
import TestTemplate
import Template

main = do print $ compose3 (\x -> x * x) 2
          print $ map4 (*2) [[[[3]]]]
          print $ $(composeMap 4) (*2) [[[[3], [4]]]]

next :: Num a => a -> [a]
next n = [2 * n, 2 * n + 1]

generateTill :: Int -> [Int]
generateTill 1 = [1]
generateTill n = concat $ ancestors : (map next $ (drop (2^(n-2) - 1) ancestors))
    where ancestors = generateTill (n-1)
  
