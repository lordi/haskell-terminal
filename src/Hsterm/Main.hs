module Main where
import Hsterm.Hsterm (runHsterm)
import Hsterm.Config (defaultTerminalConfig, TerminalConfig(..))
import Data.Colour.SRGB
import Terminal.Types (TerminalColor(..))

myConfig = defaultTerminalConfig {
    colorMap = \c -> sRGB24read $ case c of
        Black   -> "#000000"
        Red     -> "#ff6565"
        Green   -> "#93d44f"
        Yellow  -> "#eab93d"
        Blue    -> "#204a87"
        Magenta -> "#ce5c00"
        Cyan    -> "#89b6e2"
        White   -> "#cccccc"
}

main = runHsterm myConfig
