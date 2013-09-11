{-# LANGUAGE Rank2Types #-}
-- Captures how TerminalActions change the Terminal
module Terminal.Terminal (newTerminal, defaultTerm, applyAction, testTerm, scrollTerminalDown, scrollTerminalUp) where
import System.Process
import Data.Array.Diff
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

mkChar c term = TerminalChar {
                    character = c,
                    foregroundColor = currentForeground term,
                    backgroundColor = currentBackground term,
                    isBright = optionBright term,
                    isBlinking = optionBlinking term,
                    isUnderlined = optionUnderlined term
                }
mkEmptyChar = mkChar ' '

testTerm = defaultTerm
defaultTerm = newTerminal (24, 80)

newTerminal s@(rows, cols) = Terminal {
    cursorPos = (1, 1),
    rows = rows,
    cols = cols,
    inBuffer = "",
    responseBuffer = "",
    scrollingRegion = (1, rows),
    screen = array ((1, 1), s)
        [((y, x), e) | x <- [1..cols], y <- [1..rows]],
    currentForeground = defaultForegroundColor,
    currentBackground = defaultBackgroundColor,
    optionShowCursor = True,
    terminalTitle = "",
    optionBright = False,
    optionUnderlined = False,
    optionBlinking = False
} where e = mkEmptyChar (newTerminal s) -- Hail laziness

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
    clearRows [startrow] $ term {
        screen = scrollScreenUp (scrollingRegion term) s
    }

scrollTerminalDown :: Terminal -> Terminal
scrollTerminalDown term@Terminal { screen = s, scrollingRegion = r@(startrow, endrow) } =
    clearRows [endrow] $ term {
        screen = scrollScreenDown (scrollingRegion term) s
    }

clearRows :: [Int] -> Terminal -> Terminal
clearRows rows term@Terminal { screen = s } =
    term {
        screen = s // [((y_,x_), mkEmptyChar term)|x_<-[1..80],y_<-rows]
    }

clearColumns :: Int -> [Int] -> Terminal -> Terminal
clearColumns row cols term@Terminal { screen = s } =
    term { 
        screen = s // [((row,x_), mkEmptyChar term)|x_<-cols]
    }

-- Attribute mode handling
applyAttributeMode :: Terminal -> AttributeMode -> Terminal
applyAttributeMode term ResetAllAttributes =
    term {
        currentForeground = defaultForegroundColor,
        currentBackground = defaultBackgroundColor,
        optionBright = False,
        optionUnderlined = False,
        optionBlinking = False
    }
applyAttributeMode term (Foreground c) = term { currentForeground = c }
applyAttributeMode term (Background c) = term { currentBackground = c }
applyAttributeMode term ResetForeground = term { currentForeground = defaultForegroundColor }
applyAttributeMode term ResetBackground = term { currentBackground = defaultBackgroundColor }
applyAttributeMode term Bright = term { optionBright = True }
applyAttributeMode term Normal = term { optionBright = False }
applyAttributeMode term Underlined = term { optionUnderlined = True }
applyAttributeMode term NotUnderlined = term { optionUnderlined = False }
applyAttributeMode term Blinking = term { optionBlinking = True }
applyAttributeMode term NotBlinking = term { optionBlinking = False }
applyAttributeMode term other = trace ("\nUnimplemented attribute mode: " ++ show other) term

applyAction :: TerminalAction -> Terminal -> Terminal
applyAction act term@Terminal { screen = s, cursorPos = pos@(y, x) } =
    safeCursor t
    -- where t = case (trace ("Action" ++ show act) act) of
    where t = case act of
            Ignored             -> term

            -- Bell
            CharInput '\a'      -> term

            -- Tab
            CharInput '\t'      -> term { cursorPos = (y, (x `div` 8 + 1) * 8) }

            -- Newline
            CharInput '\n'      -> term { cursorPos = (y + 1, 1) }
            CharInput '\r'      -> term { cursorPos = (y, 1) }
            CharInput '\b'      -> term { screen = s // [(pos, mkEmptyChar term)], cursorPos = (y, x - 1) }
            CharInput c         -> term { 
                                    screen = s // [(pos, mkChar c term)],
                                    cursorPos = (y, x + 1) }

            -- Cursor movements
            CursorUp n          -> (iterate up term) !! n
            CursorDown n        -> (iterate down term) !! n
            CursorForward n     -> (iterate right term) !! n
            CursorBackward n    -> (iterate left term) !! n
            CursorAbsoluteColumn col -> term { cursorPos = (y, col) }
            CursorAbsoluteRow row  -> term { cursorPos = (row, x) }
            SetCursor row col   -> term { cursorPos = (row, col) }

            -- Cursor visibility
            ShowCursor s        -> term { optionShowCursor = s }

            -- Colors, yay!
            ANSIAction _ 'm'    -> term

            -- Scrolling
            SetScrollingRegion start end -> term { scrollingRegion = (start, end) }
            ScrollUp n          -> (iterate scrollTerminalUp term) !! n
            ScrollDown n        -> (iterate scrollTerminalDown term) !! n

            -- Erases the screen with the background color and moves the cursor to home.
            ANSIAction [2] 'J'  -> clearRows [1..24] $ term { cursorPos = (1, 1) }

            -- Erases the screen from the current line up to the top of the screen.
            ANSIAction [1] 'J'  -> clearRows [1..y] term

            -- Erases the screen from the current line down to the bottom of the screen.
            ANSIAction _ 'J'    -> clearRows [y..24] term

            -- Erases the entire current line.
            ANSIAction [2] 'K'  -> clearColumns y [1..80] term

            -- Erases from the current cursor position to the start of the current line.
            ANSIAction [1] 'K'  -> clearColumns y [1..x] term

            -- Erases from the current cursor position to the end of the current line. 
            ANSIAction _ 'K'    -> clearColumns y [x..80] term
           
            -- Set the terminal title
            SetTerminalTitle t  -> term { terminalTitle = t }

            -- Attribute mode / color handling
            SetAttributeMode ms -> foldl applyAttributeMode term ms

            _                   -> trace ("\nUnimplemented action: " ++ show act) term

