{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import AniMonad
import System.Exit qualified as Exit
import TH
import Test.HUnit

same :: (Eq a, Show a) => Signal a -> Signal a -> Test
same a b = frames a ~=? frames b

testFrame :: Test
testFrame = initial ~=? (frames . unframes) initial
  where
    initial = [1, 3, 5, 2] :: [Int]

testSigLens :: Test
testSigLens = same anim (get sig_lens (set sig_lens anim p))
  where
    p :: Signal (Float, Float) = pure (0, 0)
    anim = lerp 0 1
    sig_lens :: Traversal' (Signal (Float, Float)) (Signal Float)
    sig_lens = sigLens _1

testMapEnd :: Test
testMapEnd = test [end (0 |> anim1) ~?= 7, end (1 |> anim2) ~?= 14]
  where
    anim1 :: Chain Float = ky 1.0 1 <> mapEnd (const 7)
    anim2 = anim1 <> mapEnd (* 2)

testTimings :: Test
testTimings =
  test
    [ end l ~=? 1,
      end (extend 3 l) ~=? 1,
      same (stretchTo 5 l) (stretchBy 5 l)
    ]
  where
    l = lerp 0 1 :: Signal Float

data Rectish = Rectish {_w, _h :: Float, _num :: Int}

$(makeLenses ''Rectish)

testKeys :: Test
testKeys = test [same (get (sigLens h) scene) (1 |> key id 10 1 <> key id 1 1)]
  where
    scene :: Signal Rectish
    scene = Rectish 1 1 1 |> key h 10 1 <> simul [key w 1, key h 1, key num 37] 1

testIndexSignal :: Test
testIndexSignal = same (get (sigLens (ix 0)) scene) (1 |> key id 5 1)
  where
    scene :: Signal [Float]
    scene = [1, 2, 3] |> key (ix 0) 5 1

testManySignal :: Test
testManySignal = test [same (get (sigLens (ix 0)) scene) (1 |> ky 2 1), same (get (sigLens (ix 1)) scene) (1 |> ky 3 1)]
  where
    scene :: Signal [Float]
    scene = pure [1, 1, 1] & partsOf (sigLens traverse) .~ [(1 :: Float) |> ky i 1 | i <- [2 ..]]

testInstant :: Test
testInstant = same ((0 :: Float) |> ky 1 1) (0 |> ky 0 0 <> ky 1 1)

$( testAll
     [ 'testFrame,
       'testSigLens,
       'testTimings,
       'testKeys,
       'testIndexSignal,
       'testInstant,
       'testManySignal,
       'testMapEnd
     ]
 )

main :: IO ()
main = do
  result <- runTestTT allTests
  if failures result + errors result == 0 then Exit.exitSuccess else Exit.exitFailure
