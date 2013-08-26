{-# LANGUAGE Rank2Types #-}
module Terminal.Terminal (newTerminal, defaultTerm, applyAction, testTerm, scrollTerminalDown, scrollTerminalUp) where
import System.Process
import Data.Array.Unboxed
import Data.Char
import Control.Monad
import Control.Monad.State hiding (state)
import System.IO
import System.Posix.IO
import System.Posix.Terminal hiding (TerminalState)
import GHC.IO.Handle
import Debug.Trace
import Control.Concurrent
import Control.Applicative hiding (many)

import Terminal.Parser
import Terminal.Types

defaultForegroundColor = White
defaultBackgroundColor = Black

emptyChar = ' '

defaultTerm = newTerminal (24, 80)

testTerm = defaultTerm {
            screen = array
                    ((1, 1), (24,80))
                    [((y, x), chr $ ord 'A' + y) | x <- [1..80], y <- [1..24]]
            }

newTerminal s@(rows, cols) = Terminal {
    cursorPos = (1, 1),
    rows = rows,
    cols = cols,
    inBuffer = "",
    responseBuffer = "",
    scrollingRegion = (1, rows),
    screen = array
        ((1, 1), s)
        [((y, x), emptyChar) | x <- [1..cols], y <- [1..rows]],
    foreground = array
        ((1, 1), s)
        [((y, x), fromEnum defaultForegroundColor) | x <- [1..cols], y <- [1..rows]],
    background = array
        ((1, 1), s)
        [((y, x), fromEnum defaultBackgroundColor) | x <- [1..cols], y <- [1..rows]],
    currentForeground = fromEnum defaultForegroundColor,
    currentBackground = fromEnum defaultBackgroundColor,
    terminalTitle = ""
}

up t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y - 1, x) }
down t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y + 1, x) }
left t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y, x - 1) }
right t@Terminal {cursorPos = (y, x)} = safeCursor $ t { cursorPos = (y, x + 1) }

-- Wrap line
safeCursor t@Terminal {cursorPos = (y, 81) } =
    safeCursor $ t { cursorPos = (y + 1, 1) }

safeCursor t@Terminal {cursorPos = (25, x), screen = s } =
    safeCursor $ scrollTerminalDown $ t { cursorPos = (24, 1) }

safeCursor term@Terminal {cursorPos = (y, x), cols = c, rows = r } =
    term { cursorPos = (min r (max 1 y), min c (max 1 x)) }

scrollIndexUp :: (Int, Int) -> ScreenIndex -> ScreenIndex
scrollIndexUp (startrow, endrow) (y, x)
    | y > startrow && y <= endrow  = (y - 1, x)
    | y == startrow                = (endrow, x)
    | otherwise                    = (y, x)

scrollIndexDown :: (Int, Int) -> ScreenIndex -> ScreenIndex
scrollIndexDown (startrow, endrow) (y, x)
    | y >= startrow && y < endrow  = (y + 1, x)
    | y == endrow                  = (startrow, x)
    | otherwise                    = (y, x)

scrollScreenUp r@(startrow, endrow) screen =
    ixmap ((1, 1), (24, 80)) (scrollIndexUp r) screen

scrollScreenDown r@(startrow, endrow) screen =
    ixmap ((1, 1), (24, 80)) (scrollIndexDown r) screen

scrollTerminalUp :: Terminal -> Terminal
scrollTerminalUp term@Terminal { screen = s, scrollingRegion = r@(startrow, endrow) } =
    clearLines [startrow] $ term {
        screen = scrollScreenUp (scrollingRegion term) s,
        foreground = scrollScreenUp (scrollingRegion term) (foreground term),
        background = scrollScreenUp (scrollingRegion term) (background term)
    }

scrollTerminalDown :: Terminal -> Terminal
scrollTerminalDown term@Terminal { screen = s, scrollingRegion = r@(startrow, endrow) } =
    clearLines [endrow] $ term {
        screen = scrollScreenDown (scrollingRegion term) s,
        foreground = scrollScreenDown (scrollingRegion term) (foreground term),
        background = scrollScreenDown (scrollingRegion term) (background term)
    }

clearLines :: [Int] -> Terminal -> Terminal
clearLines rows term@Terminal { screen = s } =
    term {
        screen = s // [((y_,x_), emptyChar)|x_<-[1..80],y_<-rows],
        foreground = (foreground term) // [((y_,x_), fromEnum defaultForegroundColor)|x_<-[1..80],y_<-rows],
        background = (background term) // [((y_,x_), fromEnum defaultBackgroundColor)|x_<-[1..80],y_<-rows]
        }

applyAttributeMode :: Terminal -> AttributeMode -> Terminal
applyAttributeMode term ResetAllAttributes = term {
                                                currentForeground = fromEnum defaultForegroundColor,
                                                currentBackground = fromEnum defaultBackgroundColor
                                                }
applyAttributeMode term (Foreground c) = term { currentForeground = fromEnum c }
applyAttributeMode term (Background c) = term { currentBackground = fromEnum c }
applyAttributeMode term _ = term -- TODO Implement blink, reverse, underline etc.

applyAction :: TerminalAction -> Terminal -> Terminal
applyAction act term@Terminal { screen = s, cursorPos = pos@(y, x) } =
    safeCursor t
    where t = case act of
            Ignored             -> term

            -- Bell
            CharInput '\a'      -> term

            CharInput '%'       -> term { screen = scrollScreenUp (scrollingRegion term) s }
            -- Newline
            CharInput '\n'      -> term { cursorPos = (y + 1, 1) }
            CharInput '\r'      -> term { cursorPos = (y, 1) }
            CharInput '\b'      -> term { screen = s // [(pos, emptyChar)], cursorPos = (y, x - 1) }
            CharInput c         -> term { 
                                    screen = s // [(pos, c)],
                                    foreground = (foreground term) // [(pos, currentForeground term)],
                                    background = (background term) // [(pos, currentBackground term)],
                                    cursorPos = (y, x + 1) }
            CursorUp n          -> (iterate up term) !! n
            CursorDown n        -> (iterate down term) !! n
            CursorForward n     -> (iterate right term) !! n
            CursorBackward n    -> (iterate left term) !! n

            -- Colors, yay!
            ANSIAction _ 'm'    -> term

            -- Force cursor position
            SetCursor col row   -> term { cursorPos = (col, row) }

            SetScrollingRegion start end -> term { scrollingRegion = (start, end) }
            ScrollUp            -> scrollTerminalUp term
            ScrollDown          -> scrollTerminalDown term

            -- Erases the screen with the background color and moves the cursor to home.
            ANSIAction [2] 'J'  -> clearLines [1..24] $ term { cursorPos = (1, 1) }

            -- Erases the screen from the current line up to the top of the screen.
            ANSIAction [1] 'J'  -> clearLines [1..y] term

            -- Erases the screen from the current line down to the bottom of the screen.
            ANSIAction _ 'J'    -> clearLines [y..24] term

            -- Erases the entire current line.
            ANSIAction [2] 'K'  -> term { screen = s // [((y,x_), emptyChar)|x_<-[1..80]] }

            -- Erases from the current cursor position to the start of the current line.
            ANSIAction [1] 'K'  -> term { screen = s // [((y,x_), emptyChar)|x_<-[1..x]] }

            -- Erases from the current cursor position to the end of the current line. 
            ANSIAction _ 'K'  -> term { screen = s // [((y,x_), emptyChar)|x_<-[x..80]] }
            
            -- Set the terminal title
            SetTerminalTitle t  -> term { terminalTitle = t }

            -- Attribute mode / color handling
            SetAttributeMode modes -> foldl applyAttributeMode term modes

            _                 -> trace ("\nUnimplemented action: " ++ show act) term

