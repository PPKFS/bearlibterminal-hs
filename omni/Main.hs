module Main
  ( main
  ) where

import BearLibTerminal
import Control.Monad
import Control.Exception
import Data.Text ( Text )
import qualified Data.Text as T
import Omni.BasicOutput
import qualified Data.Map as M
import Omni.DefaultFont (defaultFont)
import Omni.Tilesets
import Omni.ManualCellsize

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
    TkEscape ->return ()
    TkClose -> return ()
    x -> maybe runLoop (>> runLoop) (M.lookup x entryMap )

entryPresses :: [Keycode]
entryPresses = [ Tk1 .. Tk9 ] <> [TkA .. TkK]

entryMap :: M.Map Keycode (IO ())
entryMap = M.fromList $ zip entryPresses (map snd entries)

entries :: [(Text, IO ())]
entries =
  [ ("Basic output", basicOutput)
  , ("Default font", defaultFont)
  , ("Tilesets", tilesets)
  , ("Sprites", return ()) -- TODO
  , ("Manual cellsize", manualCellsize)
  , ("Auto-generated tileset", return ())  -- TODO
  , ("Multiple fonts", return ())  -- TODO
  , ("Text alignment", return ())  -- TODO
  , ("Formatted log", return ()) -- TODO
  , ("Layers", return ()) -- TODO
  , ("Extended 1: basics", return ()) -- TODO
  , ("Extended 2: smooth scroll", return ()) -- TODO
  , ("Dynamic sprites", return ()) -- TODO
  , ("Speed", return ()) -- TODO
  , ("Input 1: keyboard", return ()) -- TODO
  , ("Input 2: mouse", return ()) -- TODO
  , ("Input 3: text input", return ()) -- TODO
  , ("Input 4: filtering", return ()) -- TODO
  , ("Window resizing", return ()) -- TODO
  , ("Examining cell contents", return ()) -- TODO
  ]

printEntry :: (Int, (Text, IO ())) -> IO ()
printEntry (i, (n, _)) = do
  let shortcut = toEnum $ if i < 9 then fromEnum '1' + i else fromEnum 'a' + (i-9)
  let col = if i `elem` ([0..2] <> [4]) then "white" else "gray"
  void $ terminalPrint 2 (1+i) (mconcat ["[color=orange]", T.singleton shortcut, ".[/color][color="<>col<>"]", n])

printEntries :: IO ()
printEntries = mapM_ printEntry (zip [0..] entries)


resetTerminal :: IO ()
resetTerminal = do
  -- TODO: I moved all the actual helper stuff to roguefunctor..
  -- todo: font:default, input filter to keyboard
  --void $ terminalSetText "" defaultWindowOptions { title = Just "Omni: menu" }
  terminalColorName "white"
