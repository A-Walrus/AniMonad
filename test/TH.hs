{-# LANGUAGE TemplateHaskellQuotes #-}

module TH where

import Language.Haskell.TH
import Test.HUnit

testAll :: [Name] -> Q [Dec]
testAll testNames = do
  let testListName = mkName "allTests"
      tests = map (\name -> AppE (AppE (VarE '(~:)) (LitE (StringL (nameBase name)))) (VarE name)) testNames
      testList = ListE tests
      funBody = NormalB (AppE (VarE 'test) testList)
      funClause = Clause [] funBody []
  return [SigD testListName (ConT ''Test), FunD testListName [funClause]]
