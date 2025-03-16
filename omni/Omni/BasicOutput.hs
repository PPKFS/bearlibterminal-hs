module Omni.BasicOutput where
import BearLibTerminal.Terminal.Set
import BearLibTerminal
import Control.Monad (forM_)
import Data.Text as T hiding (zip)

orangeText :: Text -> Text
orangeText = textColor "orange"
basicOutput :: IO ()
basicOutput = do
  terminalSetTitle "Omni: basic output"
  terminalClear
  terminalColorName "white"

  n <- width <$> terminalPrint 2 1 (textColor "orange" "1. " <> "Wide color range: ")
  let longWord :: String
      longWord = "antidisestablishmentarianism."

  forM_ (zip @Int [0..] longWord) $ \(i, c) -> do
    let factor :: Double
        factor = fromIntegral i / fromIntegral n
        red = round @_ @Int $ (1.0 - factor) * 255
        green = round $ factor * 255
    terminalColorUInt (colorFromRGB red green 0)
    terminalPut (2+n+i) 1 c

  terminalColorName "white"
  terminalPrint_ 2 3 (mconcat [orangeText "2. ", "Backgrounds: ", textColor "black" (textBkColor "gray" "grey" <> " " <> textBkColor "red" "red")])

  terminalPrint_ 2 5 (orangeText "3. " <> "Unicode support: Кириллица Ελληνικά α=β²±2°")

  terminalPrint_ 2 7 (orangeText "4. " <> "Tile composition: a + [color=red]/[/color] = a[+][color=red]/[/color], a vs. a[+][color=red]¨[/color]")

  terminalPrint_ 2 9 "[color=orange]5. [/color]Box drawing symbols:"
  terminalPrint_ 5 11 $ T.unlines
    [ "   ┌────────┐  "
    , "   │!......s└─┐"
    , "┌──┘........s.│"
    , "│............>│"
    , "│...........┌─┘"
    , "│<.@..┌─────┘  "
    , "└─────┘        "
    ]
  terminalRefresh
  c <- terminalRead
  case c of
    TkEscape -> return ()
    TkClose -> return ()
    _ -> basicOutput