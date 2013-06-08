module Main where
import Data.Monoid
import Control.Applicative ((<$>))

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Data.Array.IArray ((!))

import Terminal.Parser
import Terminal.Terminal
import Terminal.Types

handleActions [] t = t
handleActions (a : as) t = handleActions as (applyAction a t)

-- |Apply a list of `TerminalAction`s to the default terminal
applyDef actions = handleActions actions defaultTerm

-- |Test if currentForeground, currentBackground can be set
testColors :: Assertion
testColors = let term = applyDef [CharInput 'a',
                                  SetAttributeMode [Foreground Green],
                                  CharInput 'b',
                                  SetAttributeMode [Background Yellow, Blink]
                                 ] in
             toEnum <$> [currentForeground term, currentBackground term] 
             @?= [Green, Yellow]

-- |Test if colors actually have any influence on the foreground/background
-- arrays
testColors2 = TestCase (do
                let term = applyDef [SetAttributeMode [Foreground Green],
                                   SetAttributeMode [Background Magenta],
                                   CharInput 'H',
                                   SetAttributeMode [Background White],
                                   CharInput 'i',
                                   CharInput '\n',
                                   SetAttributeMode [Background Yellow, Blink],
                                   CharInput 'X',
                                   CharInput '\r',
                                   SetAttributeMode [Background Black]
                                  ]
                assertEqual "cursor is at the beginning of second row"
                    (cursorPos term) (2, 1)
                assertEqual "foreground color is correct in first char" 
                    ((foreground term) ! (1, 1)) (fromEnum Green)
                assertEqual "background color is correct in first char" 
                    ((background term) ! (1, 1)) (fromEnum Magenta)
                assertEqual "background color is correct in second char" 
                    ((background term) ! (1, 2)) (fromEnum White)
                assertEqual "foreground color is correct in second row" 
                    ((background term) ! (2, 1)) (fromEnum Yellow)
                )

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

-- |Make sure that the cursor always is within bounds
prop_SafeCursor a =
    let t = (handleActions a defaultTerm)
        (y, x) = cursorPos t in
    x >= 1 && y >= 1 && x <= cols t && y <= rows t

main :: IO ()
main = defaultMainWithOpts (
        hUnitTestToTests testColors2
        ++
       [ testCase "testColors" testColors
       , testProperty "safeCursor" prop_SafeCursor
       ]) mempty
