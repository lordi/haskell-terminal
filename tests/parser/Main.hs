module Main where
import Data.Monoid
import System.Random (getStdGen)
import Control.Applicative ((<$>))

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck hiding ((==>),)

import Terminal.Parser
import Terminal.Terminal
import Terminal.Types

-- |This section contains unit tests to validate the parser, that is to ensure
-- that the incoming character stream is correctly translated into
-- `TerminalAction`s. This fact is clearly expressed by the function signature
-- of the assertion function parsesTo which is used in the assertions below.
parsesTo :: String -> [TerminalAction] -> Assertion
parsesTo str is = let Right result = parseANSI str in result @?= (is, "")

(==>) = parsesTo

testSetCursor = "A\ESC[H\ESC[2;2H"
        ==> [CharInput 'A', SetCursor 1 1, SetCursor 2 2]

testInvalidSetCursor = "\ESC[H\ESC[2;2;X\ESC[5;1H"
        ==> [SetCursor 1 1, Ignored, SetCursor 5 1]

testMoveCursor = "A\ESC[A\ESCA\ESC[10B"
        ==> [CharInput 'A', CursorUp 1, Ignored, CursorDown 10]

testCharInput = "A2$?"
        ==> [CharInput 'A', CharInput '2', CharInput '$', CharInput '?']

testCharInputIg = "\ESC[;nw\ESC[5652;7974;10;;;xA"
        ==> [Ignored, CharInput 'w', Ignored, CharInput 'A']

testSetDisplayAttributes1 = "\ESC[0m"
        ==> [SetAttributeMode [ResetAllAttributes]]
testSetDisplayAttributes2 = "\ESC[31;40m"
        ==> [SetAttributeMode [Foreground Red, Background Black]]
testSetDisplayAttributes3 = "\ESC[37;4mU\ESC[0m"
        ==> [SetAttributeMode [Foreground White, Underscore],
                    CharInput 'U',
                    SetAttributeMode [ResetAllAttributes]]
testSetDisplayAttributes4 = "\ESC[30;5;43m"
        ==> [SetAttributeMode [Foreground Black, Blink, Background Yellow]]
testSetDisplayAttributes5 = "\ESC[1111m\ESC[50m"
        ==> [SetAttributeMode [InvalidAttributeMode], SetAttributeMode [InvalidAttributeMode]]
testSetTerminalTitle = "\ESC]0;Chickens don't clap!\007b"
        ==> [SetTerminalTitle "Chickens don't clap!", CharInput 'b']

unitTests =
       [ testCase "testSetCursor" testSetCursor
       , testCase "testInvalidSetCursor" testInvalidSetCursor
       , testCase "testMoveCursor" testMoveCursor
       , testCase "testCharInput" testCharInput
       , testCase "testCharInputIg" testCharInputIg
       , testCase "testSetDisplayAttributes1" testSetDisplayAttributes1
       , testCase "testSetDisplayAttributes2" testSetDisplayAttributes2
       , testCase "testSetDisplayAttributes3" testSetDisplayAttributes3
       , testCase "testSetDisplayAttributes4" testSetDisplayAttributes4
       , testCase "testSetDisplayAttributes5" testSetDisplayAttributes5
       , testCase "testResetColors"
            ("\ESC[39;49m" ==> [SetAttributeMode [ResetForeground, ResetBackground]])
       , testCase "testSetTerminalTitle" testSetTerminalTitle       
       ]

-- |The second section of this file consists of QuickCheck properties to ensure
-- that the peraser is rebust again arbirtrary input. Therefore a InputStream
-- type is defined which represents the incoming character stream.
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

propertyTests =
       [ testProperty "parseNotManyCharsLeftOver" prop_NotManyCharsLeftOver
       ]

-- Finally, execute the tests with the cabal test framework
main :: IO ()
main = defaultMainWithOpts (unitTests ++ propertyTests) mempty

