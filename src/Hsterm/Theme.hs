module Hsterm.Theme (colorize) where
import Terminal.Types
import Data.Maybe (fromJust)
import Data.Colour.SRGB (Colour, sRGB24read)

colorMap = [ (Black, "#000000")
    , (Red, "#ff6565")
    , (Green, "#93d44f")
    , (Yellow, "#eab93d")
    , (Blue, "#204a87")
    , (Magenta, "#ce5c00")
    , (Cyan, "#89b6e2")
    , (White, "#cccccc") ]

colorize :: TerminalColor -> Bool -> Colour
colorize termcol bold = sRGB24read . fromJust . lookup termcol cmap
        where cmap = if bold then colorMap else colorMap


