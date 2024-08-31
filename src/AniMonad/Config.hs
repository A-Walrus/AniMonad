module AniMonad.Config where
import AniMonad.Element.Base (Color)

data Config = Config {docWidth :: Int, docHeight :: Int, fps :: Int, backgroundColor :: Color}
