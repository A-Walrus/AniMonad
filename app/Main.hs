module Main where

import AniMonad
import System.Directory
import System.FilePath (replaceExtension)
import System.Process

main :: IO ()
main = writeItemsToFiles f
  where
    animation = Rect 100 100 |~ Key width 1024 1 ~> Key height 1024 1 ~> All [Key width 100, Key height 100] 1
    svgAnim = svg . draw <$> animation
    f = frames svgAnim

convertSvgToPng :: FilePath -> IO ()
convertSvgToPng svgFile = do
  let pngFile = replaceExtension svgFile "png"
  callProcess "rsvg-convert" ["-o", pngFile, svgFile]

writeItemsToFiles :: (Show a) => [a] -> IO ()
writeItemsToFiles items = do
  removeDirectoryRecursive "frames"
  createDirectory "frames"
  mapM_ writeItemToFile (zip fileNames items)
  mapM_ convertSvgToPng fileNames
  callProcess "ffmpeg" ["-y", "-framerate", show fps, "-i", "frames/%d.png", "-c:v", "libx264", "-pix_fmt", "yuv420p", "output.mp4"]
  where
    fileNames = take (length items) $ map frameFile [0 :: Int ..]
    writeItemToFile (file, item) = writeFile file (show item)
    frameFile n = "frames/" ++ show n ++ ".svg"
