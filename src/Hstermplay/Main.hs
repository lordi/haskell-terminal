-- |This program will replay the recordings of the `script` utility and
-- output the last screen.
module Main where
import System.Environment (getArgs)
import Control.Monad (foldM)
import System.IO (readFile)
import Terminal.Parser (parseString)
import Terminal.Types (Terminal)
import Terminal.Terminal (defaultTerm, applyAction)
import Terminal.Debug (printTerminal)

-- |Parse a string and apply the resulting TerminalActions to a default
-- terminal, return the resulting terminal.
playScript :: String -> Terminal
playScript s = foldl (flip applyAction) defaultTerm (parseString s)

main = getArgs >>= readFile . head >>= return . playScript >>= printTerminal
