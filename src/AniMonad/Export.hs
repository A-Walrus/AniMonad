{-# LANGUAGE OverloadedStrings #-}

module AniMonad.Export (render) where

import AniMonad.Core
import AniMonad.Element.Base
import Lucid.Svg
import System.IO (hClose, hPutStrLn)
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

passToRenderer :: [String] -> IO ()
passToRenderer items = do
  let cmd = "svg-render/target/release/svg-render"
      args = [show fps, show (length items), show docWidth, show docHeight]
      process = (proc cmd args) {std_in = CreatePipe, std_out = Inherit, std_err = Inherit}
  (Just hin, _, _, ph) <- createProcess process
  mapM_ (hPutStrLn hin . ('\n' :)) items
  hClose hin
  _ <- waitForProcess ph
  return ()

render :: (Element e) => Signal e -> IO ()
render anim = passToRenderer (frames (show . svgDoc . draw <$> anim))
