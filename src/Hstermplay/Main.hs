-- |This program will replay the recordings of the `script` utility and
-- output the last screen.
module Main where
import System.Environment (getArgs)
import Control.Monad (foldM)
import System.IO (readFile)
import Terminal.Parser (parseANSIAnnotate)
import Terminal.Types (Terminal)
import Terminal.Terminal (defaultTerm, applyAction)
import Terminal.Debug (printTerminal)

renderAnnotatedAction (action, bytes) = do
    putStrLn $ sb ++ replicate (10 - length sb) ' ' ++ "\t" ++ show action
    where sb = show bytes

-- |Parse a string and apply the resulting TerminalActions to a default
-- terminal. While at it, print each TerminalAction and the resulting
-- terminal state.
playScript :: String -> IO ()
playScript s = do
    resultTerm <- foldM applyAction' defaultTerm annotatedActions
    printTerminal resultTerm
    where (Right (annotatedActions, _)) = parseANSIAnnotate s
          applyAction' term (a, t) = do
                renderAnnotatedAction (a, t)
                return (applyAction a term)

main = getArgs >>= readFile . head >>= playScript
