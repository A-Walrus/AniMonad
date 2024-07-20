module AniMonad.Element.TH
  ( genElementInstances,
    makeElementLenses,
    genTransformTuples,
  )
where

import AniMonad.Element.Base (Element (box, draw), Transformed, combine, val)
import Control.Lens
import Data.Maybe (mapMaybe)
import Language.Haskell.TH

genElementInstances :: Int -> Q [Dec]
genElementInstances maxTupleSize = concat <$> mapM genElemntInstance [2 .. maxTupleSize]
  where
    genElemntInstance :: Int -> Q [Dec]
    genElemntInstance n = do
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

genTransformTuples :: Int -> Q [Dec]
genTransformTuples n = concat <$> mapM (uncurry genInstance) [(x, y) | x <- [2 .. n], y <- [1 .. x]]
  where
    genInstance :: Int -> Int -> Q [Dec]
    genInstance size field = do
      let names = map (\i -> mkName $ "a" ++ show i) [1 .. size]
          tupleType = foldl AppT (TupleT size) (map VarT names)
          instanceType = AppT (ConT ''Transformed) tupleType
          classType = ConT (mkName ("Field" ++ show field))
          fieldType = VarT (names !! (field - 1))
          instanceHead = AppT (AppT (AppT (AppT classType instanceType) instanceType) fieldType) fieldType
          f = mkName ("_" ++ show field)
          valField = ValD (VarP f) (NormalB (InfixE (Just (VarE 'val)) (VarE '(.)) (Just (VarE f)))) []
      sequence
        [instanceD (cxt [appT (conT ''Element) (pure tupleType)]) (pure instanceHead) [pure valField]]
