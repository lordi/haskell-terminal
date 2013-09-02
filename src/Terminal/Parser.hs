module Terminal.Parser (parseANSI, parseANSIAnnotate) where
import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Control.Monad.State
import System.IO
import System.Exit
import Data.Char
import Text.Parsec
import Text.Parsec.String
import Debug.Trace
import Data.List (insert)
import Data.Maybe (maybeToList)
import qualified Text.Parsec.Token as PT

import Terminal.Types
import Terminal.ParserUtils

-- TODO: choose another name
simplify :: TerminalAction -> TerminalAction
simplify (ANSIAction [] 'A') = CursorUp 1
simplify (ANSIAction [n] 'A') = CursorUp n
simplify (ANSIAction [] 'B')  = CursorDown 1
simplify (ANSIAction [n] 'B')  = CursorDown n
simplify (ANSIAction [] 'C') = CursorForward 1
simplify (ANSIAction [n] 'C') = CursorForward n
simplify (ANSIAction [] 'D') = CursorBackward 1
simplify (ANSIAction [n] 'D') = CursorBackward n
simplify (ANSIAction [n] 'G') = CursorAbsoluteColumn n
simplify (ANSIAction [n] 'd') = CursorAbsoluteRow n
simplify (ANSIAction [25] 'h') = ShowCursor True
simplify (ANSIAction [25] 'l') = ShowCursor False
simplify (ANSIAction [] 'H') = SetCursor 1 1
simplify (ANSIAction [] 'f') = SetCursor 1 1
simplify (ANSIAction [y,x] 'H') = SetCursor y x
simplify (ANSIAction [y,x] 'f') = SetCursor y x

simplify (ANSIAction [start, end] 'r') = SetScrollingRegion start end
simplify (ANSIAction [] 'S') = ScrollUp 1
simplify (ANSIAction [n] 'S') = ScrollUp n
simplify (ANSIAction [] 'T') = ScrollDown 1
simplify (ANSIAction [n] 'T') = ScrollDown n

simplify (ANSIAction attrModeNumbers 'm') = SetAttributeMode (map toEnum attrModeNumbers)
simplify x = x

parseString :: String -> [TerminalAction]
parseString str = fst (fromRight (parseANSI str))
                where fromRight :: Either a b -> b
                      fromRight (Right r) = r

parseANSI :: String -> Either ParseError ([TerminalAction], String)
parseANSI s = parse (manyWithLeftover pSingle) "" s

-- |Parse incoming text, and return each TerminalAction annotated with the
-- parsed string. This is usefull for debugging.
parseANSIAnnotate :: String -> Either ParseError ([(TerminalAction, String)], String)
parseANSIAnnotate s = parse (manyWithLeftover $ annotate pSingle) "" s

pSingle :: Parser TerminalAction
pSingle = (pANSISequence <|> pChar) >>= return . simplify

pANSISequence :: Parser (TerminalAction)
pANSISequence = try (pStandardANSISeq)
    <|> try (pSetTerminalTitle)
    <|> try (string "\ESCM" >> return (ScrollUp 1))
    <|> try (string "\ESCD" >> return (ScrollDown 1))
    <|> try (string "\ESC=" >> return KeypadKeysApplicationsMode)
    <|> try (string "\ESC>" >> return KeypadKeysNumericMode)
    <|> try (string "\ESC(B" >> return Ignored)
    <|> try (string "\ESC#8" >> return Ignored)
    -- Catch invalid and not implemented sequences
    <|> try (string "\ESC" >> notFollowedBy (string "]0;") >> manyTill anyNonEscapeChar (letter <|> try (char '\ESC')) >> return Ignored)

pStandardANSISeq = do
    string "\ESC["
    optionMaybe (char '?')
    param <- optionMaybe pNumber
    params <- manyUpTo 0 2 (char ';' >> pNumber)
    c <- letter
    return $ ANSIAction (maybeToList param ++ params) c

pSetTerminalTitle = do
    string "\ESC]0;"
    title <- manyTill (satisfy (/= '\007')) (try (char '\007'))
    return (SetTerminalTitle title)

anyNonEscapeChar = satisfy (/= '\ESC')

pChar :: Parser (TerminalAction)
pChar = (anyNonEscapeChar >>= return . CharInput)

pNumber = read `fmap` (manyUpTo 1 6 digit)

main = do
    print $ parseANSI "wldjawlkdj1234\a\n\n\ESC[0m\ESC[1;6m\ESC[2K\ESC[A\n12\n"
    print $ parseANSI "|M}\210\195\238\ESC[;\171\&2`[ZZZ_`__a\\a]\\aaa`_Z]["
    print $ parseANSIAnnotate "\ESC[1;6m\ESC[2K\ESC[A\n12\n"

