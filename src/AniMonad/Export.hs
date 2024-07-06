{-# LANGUAGE OverloadedStrings #-}

module AniMonad.Export (frames, unframes, fps, frameTime, render) where

import AniMonad.Core
import AniMonad.Element.Base
import Control.Monad (forM_)
import Lucid.Svg
import System.Directory
import System.FilePath (replaceExtension)
import System.FilePath.Posix ((</>))
import System.Process

fps :: Time
fps = 24

frameTime :: Time
frameTime = 1 / fps

frames :: Signal a -> [a]
frames = sample frameTime

unframes :: [a] -> Signal a
unframes = unsample frameTime

docWidth, docHeight :: Int
docWidth = 1024
docHeight = 1024

svgDoc :: Svg () -> Svg ()
svgDoc content = do
  doctype_
  with (svg11_ content) [version_ "1.1", width_ w, height_ h, viewBox_ $ nw2 <> " " <> nh2 <> " " <> w <> " " <> h]
  where
    h = showT docHeight
    w = showT docWidth
    (nh2, nw2) = (showT (-(docHeight `div` 2)), showT (-(docWidth `div` 2)))

convertSvgToPng :: FilePath -> IO ()
convertSvgToPng svgFile = do
  let pngFile = replaceExtension svgFile "png"
  callProcess "resvg" [svgFile, pngFile]

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

render :: (Element e) => Signal e -> IO ()
render anim = writeItemsToFiles f
  where
    svgAnim = svgDoc . draw <$> anim
    f = frames svgAnim