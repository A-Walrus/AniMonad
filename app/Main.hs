module Main where

import qualified AniMonad (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  AniMonad.someFunc
