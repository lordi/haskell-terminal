-- This program will replay the recordings of the `script` utility and
-- output the last screen.
module Main where
import System.Environment (getArgs)
import Control.Monad (foldM)
import System.IO (readFile)
import Terminal.Parser (parseANSI)
import Terminal.Terminal (defaultTerm, applyAction)
import Terminal.Types
import Terminal.Debug (printTerminal)

fromRight :: Either a b -> b
fromRight (Right r) = r

-- |Parse a string and apply the resulting TerminalActions to a default
-- terminal.
playScript :: String -> IO ()
playScript str = do
    let actions = fst $ fromRight $ parseANSI str
        t = foldl (flip applyAction) defaultTerm actions
    printTerminal t

main = getArgs >>= readFile . head >>= playScript
