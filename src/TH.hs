{-# LANGUAGE TemplateHaskellQuotes #-}

module TH where

import Language.Haskell.TH
import Linear
import Lucid.Svg

type Vec2 = V2 Float

class Element a where
  draw :: a -> Svg ()
  box :: a -> BoundingBox

data BoundingBox = BoundingBox Vec2 Vec2

combine :: BoundingBox -> BoundingBox -> BoundingBox
combine (BoundingBox (V2 ax1 ay1) (V2 ax2 ay2)) (BoundingBox (V2 bx1 by1) (V2 bx2 by2)) = BoundingBox (V2 (min ax1 bx1) (min ay1 by1)) (V2 (max ax2 bx2) (max ay2 by2))

genElementInstances :: Int -> Q [Dec]
genElementInstances maxTupleSize = concat <$> mapM genInstance [2 .. maxTupleSize]

genInstance :: Int -> Q [Dec]
genInstance n = do
  let names = map (\i -> mkName $ "a" ++ show i) [1 .. n]
      types = map VarT names
      tupleType = foldl AppT (TupleT n) types
      pattern = TupP (map VarP names)
      drawExp = foldr1 (\acc name -> InfixE (Just acc) (VarE '(<>)) (Just name)) (map (AppE (VarE 'draw) . VarE) names)
      boxExp = AppE (AppE (VarE 'foldr1) (VarE 'combine)) (ListE (map (AppE (VarE 'box) . VarE) names))
  (: [])
    <$> instanceD
      (cxt $ map (appT (conT ''Element) . pure) types)
      (appT (conT ''Element) $ pure tupleType)
      [ funD 'draw [clause [return pattern] (normalB (return drawExp)) []],
        funD 'box [clause [return pattern] (normalB (return boxExp)) []]
      ]
