module Main where

import AniMonad
import System.Directory
import System.FilePath (replaceExtension)
import System.Process

main :: IO ()
main = writeItemsToFiles f
  where
    animation = (Rect 100 100,Transformed (V3 (V3 1 0 100) (V3 0 1 30) (V3 0 0 1)) (Circle 0))
        |~ Key (_1 . width) 1024 1
        ~> Key (_1 . height) 1024 1
        ~> Key (_2 . inner . radius) 500 1
        ~> All [Key (_1 . width) 100, Key (_1 . height) 100] 1

    svgAnim = svgDoc . uncurry (<>) . bimap draw draw <$> animation
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
