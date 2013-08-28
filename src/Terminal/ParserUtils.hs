-- General parsec helpers
module Terminal.ParserUtils where
import Text.Parsec
import Text.Parsec.String
import Data.Maybe (catMaybes)

-- |Apply parser p and return its result together with the string that has been
-- parsed.
annotate p = do
    before <- getInput
    result <- p
    after <- getInput
    return (result, take (length before - length after) before)

-- |Apply parser p as often as possible and return the matches together with the
-- bytes that are not successfully parsed (that are left over)
manyWithLeftover p = do
    x <- many p
    i <- getInput
    return (x, i)

-- |Apply parser p at least n and up to m times
manyUpTo n m p = do
    first <- count n p
    rest <- count (m - n) (optionMaybe p) 
    return (first ++ (catMaybes rest))

