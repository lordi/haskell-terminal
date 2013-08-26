module Terminal.Types where
import Data.Array.Unboxed
import Data.Char
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)

type ScreenIndex = (Int, Int)

type TerminalArray = UArray ScreenIndex
type TerminalScreen = TerminalArray Char
type TerminalColorArray = TerminalArray Int

data Terminal = Terminal {
    cursorPos :: ScreenIndex,
    screen :: TerminalScreen,
    foreground :: TerminalColorArray,
    background :: TerminalColorArray,
    inBuffer :: String,
    responseBuffer :: String,
    terminalTitle :: String,
    scrollingRegion :: (Int, Int),
    rows :: Int,
    cols :: Int,

    currentForeground :: Int,
    currentBackground :: Int
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
     | SetTerminalTitle String
     | Ignored
     deriving (Show, Eq)

data TerminalColor =
      Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    deriving (Show, Eq)

instance Enum TerminalColor where
    fromEnum = fromJust . flip lookup tableTC
    toEnum = fromJust . flip lookup (map swap tableTC)
tableTC = [ (Black, 0)
          , (Red, 1)
          , (Green, 2)
          , (Yellow, 3)
          , (Blue, 4)
          , (Magenta, 5)
          , (Cyan, 6)
          , (White, 7)
          ]

data AttributeMode =
       InvalidAttributeMode
     | ResetAllAttributes
     | Bright
     | Dim
     | Underscore
     | Blink
     | Reverse
     | Hidden
     | Foreground TerminalColor
     | Background TerminalColor
     deriving (Show, Eq)

instance Enum AttributeMode where
    fromEnum = fromJust . flip lookup tableAM
    toEnum = (fromMaybe InvalidAttributeMode) . flip lookup (map swap tableAM)
tableAM = [ (ResetAllAttributes, 0)
          , (Bright, 1)
          , (Dim, 2)
          , (Underscore, 4)
          , (Blink, 5)
          , (Reverse, 7)
          , (Hidden, 8)
          ] ++ [(Foreground tcol, 30 + fromEnum tcol) | tcol <- [Black .. White]]
            ++ [(Background tcol, 40 + fromEnum tcol) | tcol <- [Black .. White]]

