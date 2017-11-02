{-# LANGUAGE TemplateHaskell #-}
module Template where

import Language.Haskell.TH

composeN :: Int -> Q Dec
composeN n
  | n >= 1    = funD name [cl]
  | otherwise = fail "composeN: argument n may not be <= 0"
  where
    name = mkName $ "compose" ++ show n
    func = mkName "f"
    composeAll = foldl1 (\f fs -> [| $f . $fs |])
    funcs = replicate n (varE func)
    composedF = composeAll funcs
    cl = clause [(varP func)] (normalB composedF) []
  
composeMap' :: Int -> Q Dec
composeMap' n
  | n >= 1    = funD name [cl]
  | otherwise = fail "composeMap: argument n may not be <= 0"
  where
    name = mkName $ "map" ++ show n
    composeAll = foldl1 (\fs f -> [| $fs . $f |])
    funcs = replicate n [| map |]
    composedF = composeAll funcs
    cl = clause [] (normalB composedF) []

composeMap :: Int -> Q Exp
composeMap n = do
  f <- newName "f"
  maps <- composedF
  return $ LamE [(VarP f)] (AppE maps (VarE f))
  where
    composeAll = foldl1 (\fs f -> [| $fs . $f |])
    funcs = replicate n [| map |]
    composedF = composeAll funcs
