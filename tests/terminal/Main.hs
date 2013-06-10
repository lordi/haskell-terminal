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
                                   SetAttributeMode [Foreground Yellow, Blink],
                                   CharInput 'X',
                                   SetAttributeMode  [ResetAllAttributes],
                                   CharInput '\n',
                                   CharInput 'Y',
                                   CharInput '\r'
                                  ]
                assertEqual "cursor is at the beginning of third row"
                    (cursorPos term) (3, 1)
                assertEqual "foreground color is correct in first char" 
                    ((foreground term) ! (1, 1)) (fromEnum Green)
                assertEqual "background color is correct in first char" 
                    ((background term) ! (1, 1)) (fromEnum Magenta)
                assertEqual "background color is correct in second char" 
                    ((background term) ! (1, 2)) (fromEnum White)
                assertEqual "foreground color is correct in second row" 
                    ((foreground term) ! (2, 1)) (fromEnum Yellow)
                assertEqual "background color is default in third row" 
                    ((background term) ! (3, 1)) (fromEnum Black)
                )

testClearSreen = TestCase (do
                let term = applyDef [CharInput 'H',
                                     CharInput 'i',
                                     CharInput '\n',
                                     ANSIAction [2] 'J'
                                    ]
                assertEqual "cursor is at the beginning of first row"
                    (cursorPos term) (1, 1)
                assertEqual "first char in first row is empty"
                    (screen term ! (1, 1)) ' ')

testColorsDoScroll = TestCase (do
                let term = applyDef ([
                                   SetAttributeMode [Foreground Yellow, Background Green],
                                   CharInput 'X',
                                   SetAttributeMode  [ResetAllAttributes]
                                   ] ++ (take 50 $ repeat (CharInput '\n')))
                assertEqual "cursor is at the beginning of last row"
                    (cursorPos term) (rows term, 1)
                assertEqual "background color is default in (1, 1)"
                    ((background term) ! (1, 1)) (fromEnum Black)
                assertEqual "foreground color is default in (1, 1)"
                    ((foreground term) ! (1, 1)) (fromEnum White)
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
        concat (hUnitTestToTests <$> [testColors2, testClearSreen, testColorsDoScroll])
        ++
       [ testCase "testColors" testColors
       , testProperty "safeCursor" prop_SafeCursor
       ]) mempty
