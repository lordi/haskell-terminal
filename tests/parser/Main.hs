-- |This file contains tests to validate the parser, e.g., that the
-- incoming character stream is correctly translated into
-- TerminalActions.
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

-- |An InputStream will be parsed into TerminalActions
newtype InputStream = InputStream String deriving Show

instance Arbitrary InputStream where
    arbitrary = InputStream . concat <$> (listOf1 $ oneof [
        return "\ESC[",
        return ";",
        listOf1 $ choose ('a', 'z'),
        show <$> (choose (0, 100) :: Gen Int),
        listOf1 $ choose ('\x00', '\xFF')
        ])

prop_NotManyCharsLeftOver (InputStream str) =
    let Right (x, s) = parseANSI str in
    length s < 15

testSetCursor :: Assertion
testSetCursor = let Right result = parseANSI "A\ESC[H\ESC[2;2H" in
          result @?= ([CharInput 'A', SetCursor 1 1, SetCursor 2 2], "")

testInvalidSetCursor :: Assertion
testInvalidSetCursor = let Right result = parseANSI "\ESC[H\ESC[2;2;X\ESC[5;1H" in
          result @?= ([SetCursor 1 1, Ignored, SetCursor 5 1], "")

testMoveCursor :: Assertion
testMoveCursor = let Right result = parseANSI "A\ESC[A\ESCA\ESC[10B" in
          result @?= ([CharInput 'A', CursorUp 1, Ignored, CursorDown 10], "")

main :: IO ()
main = defaultMainWithOpts
       [ testCase "testSetCursor" testSetCursor
       , testCase "testInvalidSetCursor" testInvalidSetCursor
       , testProperty "parseNotManyCharsLeftOver" prop_NotManyCharsLeftOver
       ] mempty




