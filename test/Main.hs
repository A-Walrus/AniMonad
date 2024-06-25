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

data Rect = Rect {_width, _height :: Float, _num :: Int}

$(makeLenses ''Rect)

keys :: Test
keys = frames (view (sigLens height) scene) ~=? frames (lerp 1 10 <> lerp 10 1)
  where
    scene :: Signal Rect
    scene = Rect 1 1 1 |~ Key height 10 1 ~> [Key width 1 1, Key height 1 1, Key num 37 1] 

tests :: Test
tests = test [frameTest, subSig, timings, keys]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result + errors result == 0 then Exit.exitSuccess else Exit.exitFailure
