module Omni.Tilesets where

import BearLibTerminal
import BearLibTerminal.Terminal.Print
import Control.Monad
import BearLibTerminal.Keycodes

tilesets :: IO ()
tilesets = do
  terminalComposition CompositionOn
  terminalSet_ "window.title='Omni: tilesets"
  terminalSet_ "U+E100: Media/Runic.png, size=8x16"
  terminalSet_ "U+E200: Media/Tiles.png, size=32x32, align=top-left"
  terminalSet_ "U+E300: Media/fontawesome-webfont.ttf, size=24x24, spacing=3x2, codepage=Media/fontawesome-codepage.txt"
  terminalSet_ "zodiac font: Media/Zodiac-S.ttf, size=24x36, spacing=3x3, codepage=437"

  terminalClear
  terminalColorName "white"

  terminalPrint_ 2 1 "[color=orange]1.[/color] Of course, there is default font tileset."

  terminalPrint_ 2 3 "[color=orange]2.[/color] You can load some arbitrary tiles and use them as glyphs:"
  terminalPrint_ (2+3) 4 $ "Fire rune ([color=red][U+E102][/color]), " <> "water rune ([color=lighter blue][U+E103][/color]), " <> "earth rune ([color=darker green][U+E104][/color])"

  terminalPrint_ 2 6 "[color=orange]3.[/color] Tiles are not required to be of the same size as font symbols:"
  terminalPutInt (2+3+0) 7 (0xE200+7)
  terminalPutInt (2+3+5) 7 (0xE200+8)
  terminalPutInt (2+3+10) 7 (0xE200+9)

  terminalPrint_ 2 10 "[color=orange]4.[/color] Like font characters, tiles may be freely colored and combined:"
  terminalPutInt (2+3+0) 11 (0xE200+8)
  terminalColorName "lighter orange"
  terminalPutInt (2+3+5) 11 (0xE200+8)
  terminalColorName "orange"
  terminalPutInt (2+3+10) 11 (0xE200+8)
  terminalColorName "dark orange"
  terminalPutInt (2+3+15) 11 (0xE200+8)

  terminalColorName "white"
  let order = zip [0..] [11, 10, 14, 12, 13]
  forM_ order $ \(i, o) -> do
    terminalPutInt (2+3+25+i*4) 11 (0xE200+o)
    terminalPutInt (2+3+25+(length order +1)*4) 11 (0xE200+o)
  terminalPutInt (2+3+25+length order*4) 11 (0xE200+15)

  terminalPrint_ 2 14 "[color=orange]5.[/color] And tiles can even come from TrueType fonts like this:"
  forM_ [0..5] $ \i -> terminalPutInt (2+3+i*5) 15 (0xE300+i)

  terminalPrint_ (2+3) 18 "...or like this:\n[font=zodiac]D F G S C"

  terminalRefresh

  c <- terminalRead
  case c of
    TkClose -> finish
    TkEscape -> finish
    _ -> tilesets
  where
    finish = do
      terminalSet_ "U+E100: none; U+E200: none; U+E300: none; zodiac font: none"
      terminalComposition CompositionOff