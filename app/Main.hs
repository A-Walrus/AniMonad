module Main where

import AniMonad
import System.Directory
import System.Process

main :: IO ()
main = writeItemsToFiles f
  where
    animation = Rect 100 100 |~ Key width 1024 1 ~> Key height 1024 1 ~> [Key width 100 1, Key height 100 1]
    svgAnim = svg . draw <$> animation
    f = frames svgAnim

writeItemsToFiles :: (Show a) => [a] -> IO ()
writeItemsToFiles items = do
  createDirectoryIfMissing True "frames"
  mapM_ writeItemToFile (zip [0 :: Int ..] items)
  callProcess "magick" (words "mogrify -format png frames/*.svg")
  callProcess "ffmpeg" (words "-framerate 30 -i frames/%d.png -c:v libx264 -pix_fmt yuv420p output.mp4")
  where
    writeItemToFile (i, item) = writeFile (frameFile i) (show item)
    frameFile n = "frames/" ++ show n ++ ".svg"
