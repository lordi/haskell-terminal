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
       , testProperty "parseWithoutTooMuchLeftover" prop_ParseWithoutTooMuchLeftover
       ] mempty

testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

propListRevRevId :: [Int] -> Property
propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs

-- Parser tests
newtype InputStream = InputStream String deriving Show

instance Arbitrary InputStream where
    arbitrary = InputStream . concat <$> (listOf1 $ oneof [
        return "\ESC[",
        return ";",
        listOf1 $ choose ('a', 'z'),
        show <$> (choose (0, 100) :: Gen Int),
        listOf1 $ choose ('\x00', '\xFF')
        ])

prop_ParseWithoutTooMuchLeftover (InputStream str) =
    let Right (x, s) = play str in
    length s < 15

-- TerminalAction tests
instance Arbitrary TerminalAction where
    arbitrary = oneof [
        CharInput <$> choose ('a', 'Z'),
        CursorUp <$> choose (1,5),
        CursorDown <$> choose (1,5),
        CursorForward <$> choose (1,5),
        CursorBackward <$> choose (1,5)
        ]

handleActions [] t = t
handleActions (a : as) t = handleActions as (applyAction a t)

-- |Make sure that the cursor always is within bounds
prop_SafeCursor a =
    let t = (handleActions a defaultTerm)
        (y, x) = cursorPos t in
    x >= 1 && y >= 1 && x <= cols t && y <= rows t

