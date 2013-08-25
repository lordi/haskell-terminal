-- This program will replay the recordings of the `script` utility and
-- output the last screen.
module Main where
import System.Environment (getArgs)
import Data.Array.Unboxed
import Data.Char
import Control.Monad (foldM)
import System.IO

import System.Posix.IO
import System.Posix.Terminal hiding (TerminalState)

import Terminal.Parser (parseANSI)
import Terminal.Terminal
import Terminal.Types
import Terminal.Debug (printTerminal)
import qualified Terminal.Types as T

-- |Parse a string and apply the resulting TerminalActions to a default
-- terminal.
playScript :: String -> IO ()
playScript str = do
    let playCharacter term c = do
        let -- Parse character stream
            Right (actions, leftover) = parseANSI (inBuffer term ++ [c])
            -- Apply all the actions to the terminal state
            t = foldl (flip applyAction) term actions
        return t { inBuffer = leftover }
    resultTerm <- foldM playCharacter defaultTerm str
    printTerminal resultTerm
    return ()

main = do
    getArgs >>= readFile . head >>= playScript
