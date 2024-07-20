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
  typ <- getFunctionSignature (head testNames)
  return [SigD testListName typ, FunD testListName [funClause]]

getFunctionSignature :: Name -> Q Type
getFunctionSignature name = do
  info <- reify name
  case info of
    VarI _ typ _ -> return typ
    _ -> fail "Not a function name"
