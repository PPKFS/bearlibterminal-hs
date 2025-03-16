module Omni.ManualCellsize where

import BearLibTerminal
import Control.Monad (when)
import Control.Monad.State
import Data.Functor (void)
import Formatting
import qualified Data.Vector as V

data GuiState = GuiState
  { hinting :: Int
  , size :: Int
  , cellWidth :: Int
  , cellHeight :: Int
  }

initialGui :: GuiState
initialGui = GuiState 0 12 8 16

manualCellsize :: IO ()
manualCellsize = do
  terminalSet_ "window.title='Omni: manual cellsize'"

  let font = "Media/VeraMono.ttf"
      fontHintings = V.fromList ["normal", "autohint", "none"]
      setupCellsize = do
        s <- get
        terminalSet_ (sformat ("window: cellsize=" % int % "x" % int) (cellWidth s) (cellHeight s))
      setupFont = do
        s <- get
        terminalSet_ $ sformat ("font: " % stext % ", size=" % int % ", hinting=" % stext) font (size s) (fontHintings V.! hinting s)
      runLoop = do
        s <- get
        terminalClear
        terminalColorName "white"
        terminalPrint_ 2 1 "Hello, world!"
        void $ terminalPrintString 2 3 $ "[color=orange]Font size:[/color] " <> show (size s)
        terminalPrint_ 2 4 $ "[color=orange]Font hinting:[/color] " <> (fontHintings V.! hinting s)
        terminalPrint_ 2 5 $ sformat ("[color=orange]Cell size:[/color] " % int % "x" % int) (cellWidth s) (cellHeight s)
        terminalPrint_ 2 7 "[color=orange]TIP:[/color] Use arrow keys to change cell size"
        terminalPrint_ 2 8 "[color=orange]TIP:[/color] Use Shift+Up/Down arrow keys to change font size"
        terminalPrint_ 2 9 "[color=orange]TIP:[/color] Use TAB to switch font hinting mode"

        terminalRefresh
        k <- terminalRead
        case k of
          _
            | k == TkClose || k == TkEscape -> return ()
          TkLeft
            | cellWidth s > 4 -> ifShift (return ()) (modify (\s' -> s' { cellWidth = cellWidth s - 1}) >> setupCellsize)
          TkRight
            | cellWidth s < 24 -> ifShift (return ()) (modify (\s' -> s' { cellWidth = cellWidth s + 1}) >> setupCellsize)
          TkDown -> ifShift
                      (when (size s > 4) $ modify (\s' -> s' { size = size s - 1}) >> setupFont )
                      (when (cellHeight s < 24) $ modify (\s' -> s' { cellHeight = cellHeight s + 1}) >> setupCellsize)
          TkUp -> ifShift
                      (when (size s < 64) $ modify (\s' -> s' { size = size s + 1}) >> setupFont  )
                      (when (cellHeight s > 4) $ modify (\s' -> s' { cellHeight = cellHeight s - 1}) >> setupCellsize)
          TkTab -> modify (\s' -> s' { hinting = (hinting s + 1) `mod` V.length fontHintings}) >> setupFont >> runLoop
          _ -> runLoop
      ifShift yes no = do
        shiftPressed <- terminalKeyState TkShift
        if shiftPressed then yes else no
        runLoop

  flip evalStateT initialGui $ do
    setupCellsize
    setupFont
    runLoop
  terminalSet_ "font: default; window.cellsize=auto"