{-# LANGUAGE TemplateHaskellQuotes #-}

module AniMonad.Element.TH where

import AniMonad.Element.Base (Element (box, draw), combine, Transformed, val)
import Control.Lens
import Data.Maybe (mapMaybe)
import Language.Haskell.TH

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
