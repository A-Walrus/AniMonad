{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import AniMonad
import Control.Monad (forM_)
import Data.List (sortOn)
import System.Directory
import System.FilePath (replaceExtension)
import System.FilePath.Posix ((</>))
import System.Process

main :: IO ()
main = writeItemsToFiles f
  where
    base = [at (V2 (x * 40) 0) (Rect 20 20 white) | x <- [-5 .. 5]]
    anim =
      base
        |~ simul [key (ix 4 . color) blue, key (ix 7 . color) red] 0.5
        ~> key (adjoin (ix 4) (ix 7) . y) 40 1
        ~> simul [key (ix 4 . x) (get (ix 7 . x) base), key (ix 7 . x) (get (ix 4 . x) base)] 1
        ~> key (adjoin (ix 4) (ix 7) . y) 0 1
        ~> mapEnd (sortOn (view x))
        ~> key (adjoin (ix 4) (ix 7) . color) white 0.5
        ~> sigs
          (traverse . y)
          [ 0 |~ delay (i * 0.075) ~> ky (-50) 0.3 ~> ky 0 0.3
            | i <- [0 .. 10]
          ]
    svgAnim = svgDoc . draw <$> anim
    f = frames svgAnim

convertSvgToPng :: FilePath -> IO ()
convertSvgToPng svgFile = do
  let pngFile = replaceExtension svgFile "png"
  callProcess "rsvg-convert" ["-o", pngFile, svgFile]

writeItemsToFiles :: (Show a) => [a] -> IO ()
writeItemsToFiles items = do
  createDirectoryIfMissing True "frames"
  deleteAllFilesInDirectory "frames"
  mapM_ writeItemToFile (zip fileNames items)
  mapM_ convertSvgToPng fileNames
  callProcess "ffmpeg" ["-y", "-framerate", show fps, "-i", "frames/%d.png", "-c:v", "libx264", "-pix_fmt", "yuv420p", "output.mp4"]
  where
    fileNames = take (length items) $ map frameFile [0 :: Int ..]
    writeItemToFile (file, item) = writeFile file (show item)
    frameFile n = "frames/" ++ show n ++ ".svg"

-- Function to delete all files in a directory
deleteAllFilesInDirectory :: FilePath -> IO ()
deleteAllFilesInDirectory dir = do
  -- Check if the directory exists
  exists <- doesDirectoryExist dir
  if exists
    then do
      -- Get all files in the directory
      contents <- getDirectoryContents dir
      let files = filter (`notElem` [".", ".."]) contents
      -- Delete each file
      forM_ files $ \file -> do
        let filePath = dir </> file
        removeFile filePath
    else
      putStrLn $ "Directory does not exist: " ++ dir
