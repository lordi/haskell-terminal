{-# LANGUAGE Rank2Types #-}
module Terminal.Debug (printTerminal) where
import Data.Array.Unboxed ( elems, (//) )
import System.IO
-- import Data.Set (elems)
import Terminal.Types

-- | Standard build function.
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

chunk :: Int -> [s] -> [[s]]
chunk i ls = map (take i) (build (splitter ls)) where
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n

-- |Debug function to print the current terminal state to the console
printTerminal term = do
    print $ (cursorPos term)
    putStrLn $ "," ++ (replicate (cols term) '_') ++ ","
    mapM_
        (putStrLn . (wrap "|"))
        (chunk (cols term) $ elems (screen term // [(cursorPos term, '|')]))
    putStrLn $ "`" ++ (replicate (cols term) '"') ++ "´"
    hFlush stdout
    where wrap d s = d ++ s ++ d
