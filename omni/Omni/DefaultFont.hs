module Omni.DefaultFont where
import BearLibTerminal
import BearLibTerminal.Terminal.Print
import Data.Text hiding (zipWith, length, elem, map)
import Control.Monad
import qualified Data.Vector as V
import Formatting
import Control.Monad.State
import BearLibTerminal.Keycodes

data Range = Range
  { name :: Text
  , rangeStart :: Int
  , rangeEnd :: Int
  , codes :: [Int]
  , rangeId :: Int
  }

ranges :: V.Vector Range
ranges = V.fromList $ zipWith (\i x -> x i) [0..]
  [ Range "C0 Controls and Basic Latin" 0x0020 0x007F [0x0020 .. 0x007F]
  , Range "C1 Controls and Latin-1 Supplement" 0x0080 0x00FF [0x00A0 .. 0x00FF]
  , Range "Latin Extended-A" 0x0100 0x017F [0x0100 .. 0x017F]
  , Range "Latin Extended-B" 0x0180 0x024F (0x0192 : [0x01FA .. 0x01FF])
  , Range "Spacing Modifier Letters" 0x02B0 0x02FF ([0x02C6, 0x02C7, 0x02C9] <> [0x02D8 .. 0x02DD])
  , Range "Greek and Coptic" 0x0370 0x03FF $ mconcat [ 0x037E : [0x0384 .. 0x038A], 0x038C : [0x038E .. 0x03A1], [0x03A3 .. 0x03CE]]

  , Range "Cyrillic" 0x0400 0x04FF $ [0x0400 .. 0x045F] <> [0x0490 .. 0x0491]
  , Range "Latin Extended Additional" 0x1E00 0x1EFF $ [0x1E80 .. 0x1E85] <> [0x1EF2 .. 0x1EF3]
  , Range "General Punctuation" 0x2000 0x206F $ mconcat $
        [ [0x2013 .. 0x2015]
        , [0x2017 .. 0x201E]
        , [0x2020 .. 0x2022]
        , [0x2026, 0x2030]
        , [0x2032 .. 0x2033]
        , [0x2039 .. 0x203A]
        , [0x203C, 0x203E, 0x2044]
        ]
  , Range "Superscripts and Subscript" 0x2070 0x209F [0x207F]

  , Range "Currency Symbols" 0x20A0 0x20CF $ [0x20A3 .. 0x20A4] <> [0x20A7, 0x20AC]

  , Range "Letterlike Symbols" 0x2100 0x214F [0x2105, 0x2113, 0x2116, 0x2122, 0x2126, 0x212E]

  , Range "Number Forms" 0x2150 0x218F [0x215B, 0x215E]

  , Range "Arrows" 0x2190 0x21FF $ [0x2190 .. 0x2195] <> [0x21A8]

  , Range "Mathematical Operators" 0x2200 0x22FF $ mconcat
    [ [0x2202, 0x2206, 0x220F]
    , [0x2211 .. 0x2212]
    , [0x2215]
    , [0x2219 .. 0x221A]
    , [0x221E .. 0x221F]
    , [0x2229, 0x222B, 0x2248]
    , [0x2260 .. 0x2261]
    , [0x2264 .. 0x2265]
    ]

  , Range "Miscellaneous Technical" 0x2300 0x23FF $ [0x2302, 0x2310] <> [0x2320 .. 0x2321]

  , Range "Box Drawing" 0x2500 0x257F [0x2500 .. 0x257F]

  , Range "Block Elements" 0x2580 0x259F [0x2580 .. 0x259F]

  , Range "Geometric Shapes" 0x25A0 0x25FF $ mconcat
    [ [0x25A0 .. 0x25A1]
    , [0x25AA .. 0x25AC]
    , [0x25B2, 0x25BA, 0x25BC, 0x25C4]
    , [0x25CA .. 0x25CB]
    , [0x25CF]
    , [0x25D8 .. 0x25D9]
    , [0x25E6]
    ]

  , Range "Miscellaneous Symbols" 0x2600 0x26FF $ mconcat
    [ [0x263A .. 0x263C]
    , [0x2640, 0x2642, 0x2660, 0x2663]
    , [0x2665 .. 0x2666]
    , [0x266A .. 0x266B]
    ]

  , Range "Private Use Area" 0xF000 0xF00F [0xF001 .. 0xF002]

  , Range "Alphabet presentation form" 0xFB00 0xFB0F [0xFB01 .. 0xFB02]
  ]

hOffset :: Int
hOffset = 40

defaultFont :: IO ()
defaultFont = do
  terminalSet_ "window: size=80x25, cellsize=auto, title='Omni: WGL4'; font=default"
  loop 0

loop :: Int -> IO ()
loop currentSelection = do
  terminalClear
  terminalPrint_ 2 1 "[color=white]Select unicode character range:"
  flip V.mapM_ ranges $ \range -> do
    terminalColorName (if rangeId range == currentSelection then "orange" else "light gray")
    terminalPrint_ 1 (2 + rangeId range) (sformat (stext % stext) (if rangeId range == currentSelection then "[\x203A]" else " ") (name range))
  let currRange = ranges V.! currentSelection
  forM_ [0..15] $ \j -> terminalPrint (hOffset+ 6 + j*2) 1 (sformat ("[color=orange]" % hex) j)

  flip evalStateT 0 $ forM_ [rangeStart currRange .. rangeEnd currRange] $ \code -> do
    y <- get
    when (code `mod` 16 == 0) $ terminalPrint_ hOffset (2+y) (sformat ("[color=orange]" % hexPrefix 4 % ":") code)
    let included = code `elem` codes currRange
    terminalColorName (if included then "white" else "dark gray")
    terminalPutInt (hOffset + 6 + (code `mod` 16) *2) (2+y) code
    when ((code+1) `mod` 16 == 0) $ modify (+ 1)

  terminalColorName "white"
  terminalPrint_ hOffset 20 "[color=orange]TIP:[/color] Use ↑/↓ keys to select range"
  terminalPrint_ hOffset 22 "[color=orange]NOTE:[/color] Character code points printed in\ngray are not included in the WGL4 set."

  terminalRefresh
  c <- terminalRead
  case c of
    x
      | x `elem` [TkEscape, TkClose] -> return ()
    TkUp -> when (currentSelection > 0) $ loop (currentSelection - 1)
    TkDown -> when (currentSelection < length ranges - 1) $ loop (currentSelection + 1)
    _ -> loop currentSelection
