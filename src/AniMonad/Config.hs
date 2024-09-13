module AniMonad.Config where
import Data.Colour (Colour)

data Config = Config {docWidth :: Int, docHeight :: Int, fps :: Int, backgroundColor :: Colour Float}
