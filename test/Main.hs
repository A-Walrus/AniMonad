module Main (main) where

import AniMonad
import Control.Lens
import System.Exit qualified as Exit
import Test.HUnit

frameTest :: Test
frameTest = initial ~=? (frames . unframes) initial
  where
    initial = [1, 3, 5, 2] :: [Int]

subSig :: Test
subSig = frames anim ~=? frames (view sig_lens x)
  where
    p :: Signal (Float, Float) = pure (0, 0)
    anim = lerp 0 1
    sig_lens :: Lens' (Signal (Float,Float)) (Signal Float) 
    sig_lens = sigLens _1
    x = set sig_lens anim p

tests :: Test
tests = test [frameTest, subSig]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result + errors result == 0 then Exit.exitSuccess else Exit.exitFailure
