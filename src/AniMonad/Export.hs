{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module AniMonad.Export (render) where

import AniMonad.Config
import AniMonad.Core
import AniMonad.Element.Base
import Data.List (intercalate)
import Lucid.Svg
import System.IO (hClose, hPutStrLn)
import System.Process

svgDoc :: (?config :: Config) => Svg () -> Svg ()
svgDoc content = do
  doctype_
  with (svg11_ content) [version_ "1.1", width_ w, height_ h, viewBox_ $ nw2 <> " " <> nh2 <> " " <> w <> " " <> h]
  where
    height = docHeight ?config
    width = docWidth ?config
    h = showT height
    w = showT width
    (nh2, nw2) = (showT (-(height `div` 2)), showT (-(width `div` 2)))

passToRenderer :: (?config :: Config) => [String] -> IO ()
passToRenderer items = do
  let cmd = "svg-render/target/release/svg-render"
      args = [show (fps ?config), show (length items), show (docWidth ?config), show (docHeight ?config)]
      process = (proc cmd args) {std_in = CreatePipe, std_out = Inherit, std_err = Inherit}
  (Just hin, _, _, ph) <- createProcess process
  hPutStrLn hin (intercalate "\n\n" items)
  hClose hin
  _ <- waitForProcess ph
  return ()

render :: (?config :: Config, Element e) => Signal e -> IO ()
render anim = passToRenderer (frames (show . svgDoc . draw <$> anim))
