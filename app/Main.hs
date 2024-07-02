module Main where

import AniMonad
import System.Directory
import System.FilePath (replaceExtension)
import System.Process

main :: IO ()
main = writeItemsToFiles f
  where
    animation =
      (Rect 100 100 (sRGB24read "#20aaf5"), Circle 0 blue)
        |~ Key (_1 . width) 1024 1
        ~> Key (_1 . height) 1024 1
        ~> Key (_2 . radius) 500 1
        ~> All [Key (_2 . radius) 0, Key (_2 . color) black] 1
        ~> All [Key (_1 . width) 100, Key (_1 . height) 100, Key (_1 . color) white] 1

    svgAnim = svgDoc . draw <$> animation
    f = frames svgAnim

convertSvgToPng :: FilePath -> IO ()
convertSvgToPng svgFile = do
  let pngFile = replaceExtension svgFile "png"
  callProcess "rsvg-convert" ["-o", pngFile, svgFile]

writeItemsToFiles :: (Show a) => [a] -> IO ()
writeItemsToFiles items = do
  createDirectoryIfMissing True "frames"
  mapM_ writeItemToFile (zip fileNames items)
  mapM_ convertSvgToPng fileNames
  callProcess "ffmpeg" ["-y", "-framerate", show fps, "-i", "frames/%d.png", "-c:v", "libx264", "-pix_fmt", "yuv420p", "output.mp4"]
  where
    fileNames = take (length items) $ map frameFile [0 :: Int ..]
    writeItemToFile (file, item) = writeFile file (show item)
    frameFile n = "frames/" ++ show n ++ ".svg"
