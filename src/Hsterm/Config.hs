module Hsterm.Config where
import Data.Colour.SRGB (sRGB)
import Data.Colour (Colour)
import Terminal.Types (TerminalColor(..))

defaultColor Black   = sRGB 0 0 0
defaultColor Green   = sRGB 0 1 0
defaultColor Yellow  = sRGB 0 1 1
defaultColor Blue    = sRGB 0 0 1
defaultColor Magenta = sRGB 1 1 0
defaultColor Cyan    = sRGB 1 0 1
defaultColor White   = sRGB 1 1 1

data TerminalConfig = TerminalConfig {
    defaultForegroundColor :: Colour Double,
    defaultBackgroundColor :: Colour Double,
    cursorColor :: Colour Double,
    colorMap :: TerminalColor -> Colour Double,
    fontPath :: FilePath,
    initScriptPath :: FilePath,
    fontSize :: Integer
}

defaultTerminalConfig :: TerminalConfig
defaultTerminalConfig = TerminalConfig {
    defaultForegroundColor = defaultColor White,
    defaultBackgroundColor = defaultColor Black,
    cursorColor = defaultColor White,
    fontPath = "data/fonts/monofur/monof55.ttf",
    colorMap = defaultColor,
    initScriptPath = "data/init.sh",
    fontSize = 20
}


