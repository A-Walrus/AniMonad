{-# LANGUAGE TemplateHaskellQuotes #-}

module TH where

import Control.Lens
import Data.Maybe (mapMaybe)
import Language.Haskell.TH
import Linear hiding (trace)
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
      [ funD 'draw [clause [pure pattern] (normalB (pure drawExp)) []],
        funD 'box [clause [pure pattern] (normalB (pure boxExp)) []]
      ]

type Transform = M33 Float

data Transformed a = (Element a) => Transformed {_transform :: Transform, _val :: a}

val :: Lens' (Transformed a) a
val = lens (\(Transformed _ a) -> a) (\(Transformed t _) a -> Transformed t a)

transform :: Lens' (Transformed a) Transform
transform = lens (\(Transformed t _) -> t) (\(Transformed _ a) t -> Transformed t a)

makeElementLenses :: Name -> Q [Dec]
makeElementLenses name = do
  thing <- makeFieldsNoPrefix name
  let y = mapMaybe getInstance thing
  let new = map newInstance y
  pure thing <> pure new
  where
    getInstance :: Dec -> Maybe (Cxt, Type, [Dec])
    getInstance (InstanceD _ c typ decs) = Just (c, typ, decs)
    getInstance _ = Nothing
    newInstance :: (Cxt, Type, [Dec]) -> Dec
    newInstance (ctx, AppT (AppT a b) c, decs) = InstanceD Nothing ctx (AppT (AppT a (AppT (ConT ''Transformed) b)) c) (map newMethod decs)
    newInstance _ = undefined

    newMethod :: Dec -> Dec
    newMethod (FunD n _) = FunD n [Clause [] (NormalB (InfixE (Just (VarE 'val)) (VarE '(.)) (Just (VarE n)))) []]
    newMethod e = e
