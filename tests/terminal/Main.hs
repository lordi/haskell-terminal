module Main where
import Data.Monoid
import Control.Applicative ((<$>))

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Terminal.Parser
import Terminal.Terminal
import Terminal.Types

main :: IO ()
main = defaultMainWithOpts
       [ testCase "rev" testRev
       , testProperty "safeCursor" prop_SafeCursor
       ] mempty

testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

-- TerminalAction tests
instance Arbitrary TerminalAction where
    arbitrary = oneof [
        CharInput <$> choose ('a', 'Z'),
        CursorUp <$> choose (1,50),
        CursorDown <$> choose (1,50),
        CursorForward <$> choose (1,50),
        CursorBackward <$> choose (1,50),
        SetCursor 34 <$> choose (1, 112)
        ]

handleActions [] t = t
handleActions (a : as) t = handleActions as (applyAction a t)

-- |Make sure that the cursor always is within bounds
prop_SafeCursor a =
    let t = (handleActions a defaultTerm)
        (y, x) = cursorPos t in
    x >= 1 && y >= 1 && x <= cols t && y <= rows t

