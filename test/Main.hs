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
subSig = same anim (get sig_lens (set sig_lens anim p))
  where
    p :: Signal (Float, Float) = pure (0, 0)
    anim = lerp 0 1
    sig_lens :: Traversal' (Signal (Float, Float)) (Signal Float)
    sig_lens = sigLens _1

mapEnd :: Test
mapEnd = end anim ~?= 7
  where
    anim = (0 :: Float) |~ ky 1.0 1 ~> MapEnd (const 7)

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
keys = test [same (get (sigLens h) scene) (1 |~ key id 10 1 ~> key id 1 1)]
  where
    scene :: Signal Rectish
    scene = Rectish 1 1 1 |~ key h 10 1 ~> simul [key w 1, key h 1, key num 37] 1

indexSignal :: Test
indexSignal = same (get (sigLens (ix 0)) scene) (1 |~ key id 5 1)
  where
    scene :: Signal [Float]
    scene = [1, 2, 3] |~ key (ix 0) 5 1

manySignal :: Test
manySignal = test [same (get (sigLens (ix 0)) scene) (1 |~ ky 2 1), same (get (sigLens (ix 1)) scene) (1 |~ ky 3 1)]
  where
    scene :: Signal [Float]
    scene = pure [1, 1, 1] & partsOf (sigLens traverse) .~ [(1 :: Float) |~ ky i 1 | i <- [2 ..]]

instant :: Test
instant = same ((0 :: Float) |~ ky 1 1) (0 |~ ky 0 0 ~> ky 1 1)

tests :: Test
tests = test [frameTest, subSig, timings, keys, indexSignal, instant, manySignal, mapEnd]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result + errors result == 0 then Exit.exitSuccess else Exit.exitFailure
