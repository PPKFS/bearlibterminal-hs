module BearLibTerminal.Keycodes
  ( Keycode(..)
  , intToKeycode
  ) where

import GHC.Generics
import Data.Function ((&))

-- | Set one or more of the configuration options, given as a `Text`. You should prefer this one.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
data Keycode =
  TkA | TkB | TkC | TkD | TkE | TkF | TkG | TkH | TkI | TkJ | TkK | TkL | TkM
  | TkN | TkO | TkP | TkQ | TkR | TkS | TkT | TkU | TkV | TkW | TkX | TkY | TkZ
  | Tk1 | Tk2 | Tk3 | Tk4 | Tk5 | Tk6 | Tk7 | Tk8 | Tk9 | Tk0
  | TkEnter | TkEsc | TkBackspace | TkTab | TkSpace | TkMinus | TkEquals | TkLeftBracket
  | TkRightBracket | TkBackslash | TkSemicolon | TkApostrophe | TkGrave | TkComma | TkPeriod | TkSlash
  | Tk0x39 -- this seems to be missing from BLT, but adding it as a dummy makes the enum instance easier
  | TkF1 | TkF2 | TkF3 | TkF4 | TkF5 | TkF6 | TkF7 | TkF8 | TkF9 | TkF10
  | TkF11 | TkF12 | Tk0x46 | Tk0x47 | TkPause | TkInsert | TkHome | TkPageUp | TkDelete | TkPageDown
  | TkRight | TkLeft | TkDown | TkUp
  | Tk0x53 -- see also, Tk0x39
  | TkKPDivide | TkKPMultiply | TkKPMinus | TkKPPlus
  | TkKPEnter | TkKP1 | TkKP2 | TkKP3 | TkKP4 | TkKP5 | TkKP6 | TkKP7 | TkKP8 | TkKP9 | TkKP0
  -- there's a gap of 7 here
  | TkKPPeriod | TkShift | TkControl | TkAlt
  deriving stock (Eq, Ord, Generic, Show, Bounded, Enum)

safeToEnum :: forall t . (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i =
  if (i >= fromEnum (minBound :: t)) && (i <= fromEnum (maxBound :: t))
    then Just . toEnum $ i
    else Nothing

-- | Set one or more of the configuration options, given as a `Text`. You should prefer this one.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
intToKeycode :: Int -> Keycode
intToKeycode = \case
  0x70 -> TkShift
  0x71 -> TkControl
  0x72 -> TkAlt
  i -> (safeToEnum . (\x -> x - 4) $ i) & \case
    Nothing -> error $ "unknown keycode: " <> show i
    Just w -> w