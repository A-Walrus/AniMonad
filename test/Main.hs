module Main (main) where

import AniMonad
import System.Exit qualified as Exit
import Test.HUnit

frameTest :: Test
frameTest = initial ~=? (frames . unframes) initial
  where
    initial = [1, 3, 5, 2] :: [Int]

tests :: Test
tests = test [frameTest]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result + errors result == 0 then Exit.exitSuccess else Exit.exitFailure
