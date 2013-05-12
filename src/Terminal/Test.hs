module Test where
import Test.QuickCheck
import Control.Applicative ((<$>))
import Terminal.Parser
import Terminal.Terminal
import Terminal.Types

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

prop_SafeCursor a =
    let t = (handleActions a defaultTerm)
        (y, x) = cursorPos t in
    x >= 1 && y >= 1 && x <= cols t && y <= rows t

main = do
    quickCheck prop_SafeCursor
    quickCheck prop_ParseWithoutTooMuchLeftover
