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
    x -> do
      print x
      loop currentSelection
