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
                                  SetAttributeMode [Background Yellow, Blinking]
                                 ] in
             [currentForeground term, currentBackground term] @?= [Green, Yellow]

-- |Test if currentForeground, currentBackground can be set
testBackgroundDel = TestCase (do
                    let term = applyDef [
                            SetAttributeMode [Background Yellow],
                            ANSIAction [2] 'J',
                            SetAttributeMode [Background White],
                            CharInput 'a',
                            SetAttributeMode [Background Green],
                            ANSIAction [] 'K'
                            ]
                    assertEqual "background color in the middle is yellow" 
                        (backgroundColor $ (screen term) ! (10, 10)) (Yellow)
                    assertEqual "background color in the middle is yellow" 
                        (backgroundColor $ (screen term) ! (10, 1)) (Yellow)
                    assertEqual "background color is white at (1,1)" 
                        (backgroundColor $ (screen term) ! (1, 1)) (White)
                    assertEqual "background color is green till the end of line" 
                        (backgroundColor $ (screen term) ! (1, 2)) (Green)
                    assertEqual "background color is green till the end of line" 
                        (backgroundColor $ (screen term) ! (1, 50)) (Green))

-- |Test if colors actually have any influence on the foreground/background
-- arrays
testColors2 = TestCase (do
                let term = applyDef [SetAttributeMode [Foreground Green],
                                   SetAttributeMode [Background Magenta],
                                   CharInput 'H',
                                   SetAttributeMode [Background White],
                                   CharInput 'i',
                                   CharInput '\n',
                                   SetAttributeMode [Foreground Yellow, Blinking],
                                   CharInput 'X',
                                   SetAttributeMode  [ResetAllAttributes],
                                   CharInput '\n',
                                   CharInput 'Y',
                                   CharInput '\r'
                                  ]
                assertEqual "cursor is at the beginning of third row"
                    (cursorPos term) (3, 1)
                assertEqual "foreground color is correct in first char" 
                    (foregroundColor $ (screen term) ! (1, 1)) (Green)
                assertEqual "background color is correct in first char" 
                    (backgroundColor $ (screen term) ! (1, 1)) (Magenta)
                assertEqual "background color is correct in second char" 
                    (backgroundColor $ (screen term) ! (1, 2)) (White)
                assertEqual "foreground color is correct in second row" 
                    (foregroundColor $ (screen term) ! (2, 1)) (Yellow)
                assertEqual "background color is default in third row" 
                    (backgroundColor $ (screen term) ! (3, 1)) (Black)
                )

testSetTerminalTitle = TestCase (do
                let s = "There is always money in the banana stand"
                    term = applyDef [SetTerminalTitle "x", CharInput 'a', SetTerminalTitle s]
                assertEqual "title is set"
                    (terminalTitle term) s)

testTabCharacter = TestCase (do
                let term = applyDef [CharInput '\t', CharInput 'i']
                assertEqual "cursor is at (1, 9)" (cursorPos term) (1, 9))

testClearSreen = TestCase (do
                let term = applyDef [CharInput 'H',
                                     CharInput 'i',
                                     CharInput '\n',
                                     ANSIAction [2] 'J'
                                    ]
                assertEqual "cursor is at the beginning of first row"
                    (cursorPos term) (1, 1)
                assertEqual "first char in first row is empty"
                    (character $ screen term ! (1, 1)) ' ')

testColorsDoScroll = TestCase (do
                let term = applyDef ([
                                   SetAttributeMode [Foreground Yellow, Background Green],
                                   CharInput 'X',
                                   SetAttributeMode  [ResetAllAttributes]
                                   ] ++ (take 50 $ repeat (CharInput '\n')))
                assertEqual "cursor is at the beginning of last row"
                    (cursorPos term) (rows term, 1)
                assertEqual "background color is default in (1, 1)"
                    (backgroundColor $ (screen term) ! (1, 1)) Black
                assertEqual "foreground color is default in (1, 1)"
                    (foregroundColor $ (screen term) ! (1, 1)) White
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
        concat (hUnitTestToTests <$> hUnitTests)
        ++
       [ testCase "testColors" testColors
       , testProperty "safeCursor" prop_SafeCursor
       ]) mempty
       where hUnitTests = [testColors2, testClearSreen, testColorsDoScroll,
                            testSetTerminalTitle, testTabCharacter, testBackgroundDel]
