module Terminal.Types where
import Data.Array.Unboxed
import Data.Char
import Data.Maybe (fromJust)
import Data.Tuple (swap)

type ScreenIndex = (Int, Int)

type TerminalScreen = UArray ScreenIndex Char

data Terminal = Terminal {
    cursorPos :: ScreenIndex,
    screen :: TerminalScreen,
    inBuffer :: String,
    responseBuffer :: String,
    scrollingRegion :: (Int, Int),
    rows :: Int,
    cols :: Int
}

data TerminalAction =
       CharInput Char

     -- Cursor movements
     | CursorUp Int
     | CursorDown Int
     | CursorForward Int
     | CursorBackward Int
     | SetCursor Int Int

     -- Scrolling
     | SetScrollingRegion Int Int
     | ScrollUp
     | ScrollDown

     | ANSIAction [Int] Char
     | KeypadKeysApplicationsMode
     | KeypadKeysNumericMode
     | SetAttributeMode [AttributeMode]
     | Ignored
     deriving (Show, Eq)

data AttributeMode =
       ResetAllAttributes
     | Bright
     | Dim
     | Underscore
     | Blink
     | Reverse
     | Hidden
     | ForegroundBlack
     | ForegroundRed
     | ForegroundGreen
     | ForegroundYellow
     | ForegroundBlue
     | ForegroundMagenta
     | ForegroundCyan
     | ForegroundWhite
     | BackgroundBlack
     | BackgroundRed
     | BackgroundGreen
     | BackgroundYellow
     | BackgroundBlue
     | BackgroundMagenta
     | BackgroundCyan
     | BackgroundWhite
     deriving (Show, Eq)

instance Enum AttributeMode where
    fromEnum = fromJust . flip lookup table
    toEnum = fromJust . flip lookup (map swap table)
table = [ (ResetAllAttributes, 0)
        , (Bright, 1)
        , (Dim, 2)
        , (Underscore, 4)
        , (Blink, 5)
        , (Reverse, 7)
        , (Hidden, 8)
        , (ForegroundBlack, 30)
        , (ForegroundRed, 31)
        , (ForegroundGreen, 32)
        , (ForegroundYellow, 33)
        , (ForegroundBlue, 34)
        , (ForegroundMagenta, 35)
        , (ForegroundCyan, 36)
        , (ForegroundWhite, 37)
        , (BackgroundBlack, 40)
        , (BackgroundRed, 41)
        , (BackgroundGreen, 42)
        , (BackgroundYellow, 43)
        , (BackgroundBlue, 44)
        , (BackgroundMagenta, 45)
        , (BackgroundCyan, 46)
        , (BackgroundWhite, 47)
        ]

