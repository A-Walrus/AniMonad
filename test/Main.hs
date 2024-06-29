{-# LANGUAGE TemplateHaskell #-}

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
    sig_lens :: Lens' (Signal (Float, Float)) (Signal Float)
    sig_lens = sigLens _1
    x = set sig_lens anim p

timings :: Test
timings =
  test
    [ end l ~=? 1,
      end (extend 3 l) ~=? 1,
      frames (stretchTo 5 l) ~=? frames (stretch 5 l)
    ]
  where
    l = lerp 0 1 :: Signal Float

data Rectish = Rectish {_w, _h :: Float, _num :: Int}

$(makeLenses ''Rectish)

keys :: Test
keys = test [frames (view (sigLens h) scene) ~=? frames (1 |~ Key id 10 1 ~> Key id 1 1)]
  where
    scene :: Signal Rectish
    scene = Rectish 1 1 1 |~ Key h 10 1 ~> All [Key w 1, Key h 1, Key num 37] 1

tests :: Test
tests = test [frameTest, subSig, timings, keys]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result + errors result == 0 then Exit.exitSuccess else Exit.exitFailure
