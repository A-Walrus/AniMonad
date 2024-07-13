{-# LANGUAGE OverloadedStrings #-}

module AniMonad.Export (render) where

import AniMonad.Core
import AniMonad.Element.Base 
import Control.Monad (forM_)
import Lucid.Svg
import System.Directory
import System.FilePath.Posix ((</>))
import System.Process


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

writeItemsToFiles :: (Show a) => [a] -> IO ()
writeItemsToFiles items = do
  createDirectoryIfMissing True "frames"
  deleteAllFilesInDirectory "frames"
  mapM_ writeItemToFile (zip fileNames items)
  callProcess "svg-render/target/release/svg-render" ["frames", show fps, show (length items), show docWidth, show docHeight]
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
    (Signal f) = svgAnim
