module Main where

import BearLibTerminal
import Control.Monad
import Control.Exception
import Data.Text ( Text )
import qualified Data.Text as T
import Omni.BasicOutput

main :: IO ()
main = do
  bracket_
    (terminalOpen_ >> resetTerminal)
    terminalClose
    runLoop

runLoop :: IO ()
runLoop = do
  terminalClear
  printEntries
  void $ terminalPrint 2 23 "[color=orange]ESC.[/color] Exit"
  void $ terminalPrintExt 77 22 0 0 (Just AlignRight) "library version 0.1.0.0"
  void $ terminalPrintExt 77 23 0 0 (Just AlignRight) "BearLibTerminal (Hello from Haskell!)"
  terminalRefresh
  c <- terminalRead
  case c of
    Keypress TkEsc -> return ()
    WindowEvent WindowClose -> return ()
    Keypress Tk1 -> runUntilEsc basicOutput
    x -> do
      print x
      runLoop

runUntilEsc :: IO () -> IO ()
runUntilEsc f = do
  f
  c <- terminalRead
  case c of
    WindowEvent WindowClose -> return ()
    Keypress TkEsc -> runLoop
    x -> do
      print x
      runUntilEsc f
entries :: [(Text, IO ())]
entries =
  [ ("Basic output", return ())
  , ("Default font", return ())
  , ("Tilesets", return ())
  , ("Sprites", return ())
  , ("Manual cellsize", return ())
  , ("Auto-generated tileset", return ())
  , ("Multiple fonts", return ())
  , ("Text alignment", return ())
  , ("Formatted log", return ())
  , ("Layers", return ())
  , ("Extended 1: basics", return ())
  , ("Extended 2: smooth scroll", return ())
  , ("Dynamic sprites", return ())
  , ("Speed", return ())
  , ("Input 1: keyboard", return ())
  , ("Input 2: mouse", return ())
  , ("Input 3: text input", return ())
  , ("Input 4: filtering", return ())
  , ("Window resizing", return ())
  , ("Examining cell contents", return ())
  ]

printEntry :: (Int, (Text, IO ())) -> IO ()
printEntry (i, (n, _)) = do
  let shortcut = toEnum $ if i < 9 then fromEnum '1' + i else fromEnum 'a' + (i-9)
  void $ terminalPrint 2 (1+i) (mconcat ["[color=orange]", T.singleton shortcut, ".[/color][color=gray]", n])

printEntries :: IO ()
printEntries = mapM_ printEntry (zip [0..] entries)

alignRight :: Int
alignRight = 2

resetTerminal :: IO ()
resetTerminal = do
  -- TODO: I moved all the actual helper stuff to roguefunctor..
  -- todo: font:default, input filter to keyboard
  --void $ terminalSetText "" defaultWindowOptions { title = Just "Omni: menu" }
  terminalColorName "white"
