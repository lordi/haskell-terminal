module Terminal.Types where
import Data.Array.Unboxed
import Data.Char

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
     | Ignored
     deriving (Show, Eq)

