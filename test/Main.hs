{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import AniMonad
import System.Exit qualified as Exit
import Test.HUnit

same :: (Eq a, Show a) => Signal a -> Signal a -> Test
same a b = frames a ~=? frames b

frameTest :: Test
frameTest = initial ~=? (frames . unframes) initial
  where
    initial = [1, 3, 5, 2] :: [Int]

subSig :: Test
subSig = same anim (view sig_lens (set sig_lens anim p))
  where
    p :: Signal (Float, Float) = pure (0, 0)
    anim = lerp 0 1
    sig_lens :: Traversal' (Signal (Float, Float)) (Signal Float)
    sig_lens = sigLens _1

timings :: Test
timings =
  test
    [ end l ~=? 1,
      end (extend 3 l) ~=? 1,
      same (stretchTo 5 l) (stretch 5 l)
    ]
  where
    l = lerp 0 1 :: Signal Float

data Rectish = Rectish {_w, _h :: Float, _num :: Int}

$(makeLenses ''Rectish)

keys :: Test
keys = test [same (view (sigLens h) scene) (1 |~ Key id 10 1 ~> Key id 1 1)]
  where
    scene :: Signal Rectish
    scene = Rectish 1 1 1 |~ Key h 10 1 ~> All [Key w 1, Key h 1, Key num 37] 1

indexSignal :: Test
indexSignal = same (view (sigLens (ix 0)) scene) (1 |~ Key id 5 1)
  where
    scene :: Signal [Float]
    scene = [1, 2, 3] |~ Key (ix 0) 5 1

manySignal :: Test
manySignal = test [same (view (sigLens (ix 0)) scene) (1 |~ Key id 2 1), same (view (sigLens (ix 1)) scene) (1 |~ Key id 3 1)]
  where
    scene :: Signal [Float]
    scene = pure [1, 1, 1] & partsOf (sigLens traverse) .~ [(1 :: Float) |~ Key id i 1 | i <- [2 ..]]

instant :: Test
instant = same ((0 :: Float) |~ Key id 1 1) (0 |~ Key id 0 0 ~> Key id 1 1)

tests :: Test
tests = test [frameTest, subSig, timings, keys, indexSignal, instant, manySignal]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result + errors result == 0 then Exit.exitSuccess else Exit.exitFailure
