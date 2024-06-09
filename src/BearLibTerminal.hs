{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BearLibTerminal
  ( module BearLibTerminal.Raw
  , BearLibConfigString(..)
  , WindowOptions(..)
  , Event(..)
  , Keycode(..)
  , BlockingMode(..)
  , WindowEvent(..)
  , handleEvents
  , terminalRead
  , initWindow
  , withWindow
  , defaultWindowOptions
  ) where

import BearLibTerminal.Raw
import Data.Text ( Text )
import GHC.Generics
import qualified Data.Text.Lazy.Builder as LT
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Data.Functor (void)
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import UnliftIO
import Data.Function ((&))

class BearLibConfigString s where
  toConfigString :: s -> LT.Builder

data Cellsize = Auto | Size (Int, Int)
  deriving stock (Eq, Ord, Show, Generic)

instance BearLibConfigString (Int, Int) where
  toConfigString (x, y) = LT.fromString (show x) <> LT.singleton 'x' <> LT.fromString (show y)

instance BearLibConfigString Cellsize where
  toConfigString Auto = LT.fromText "auto"
  toConfigString (Size s) = toConfigString s

instance BearLibConfigString Text where
  toConfigString s = LT.singleton '"' <> LT.fromText (T.replace "\"" "\"\"" s) <> LT.singleton '"'

instance BearLibConfigString String where
  toConfigString s = LT.singleton '"' <> LT.fromText (T.replace "\"" "\"\"" $ T.pack s) <> LT.singleton '"'

instance BearLibConfigString Bool where
  toConfigString True = LT.fromText "true"
  toConfigString False = LT.fromText "false"

newtype ConfigOption = ConfigOption { unConfig :: (Text, LT.Builder) }

instance BearLibConfigString ConfigOption where
  toConfigString (ConfigOption (t, v)) = LT.fromText t <> LT.singleton '=' <> v

toByteString :: BearLibConfigString c => c -> BS.ByteString
toByteString = BS.toStrict . LT.encodeUtf8 . LT.toLazyText . toConfigString

terminalSet :: MonadIO m => BearLibConfigString c => c -> m Bool
terminalSet = terminalSetText . TL.toStrict . LT.toLazyText . toConfigString

data WindowOptions = WindowOptions
  { size :: Maybe (Int, Int)
  , cellsize :: Maybe Cellsize
  , title :: Maybe Text
  , icon :: Maybe FilePath
  , resizeable :: Maybe Bool
  , fullscreen :: Maybe Bool
  } deriving stock (Show, Eq, Ord)

defaultWindowOptions :: WindowOptions
defaultWindowOptions = WindowOptions
  { size = Just (80, 25)
  , cellsize = Just Auto
  , title = Just "BearLibTerminalExtras"
  , icon = Nothing
  , resizeable = Just False
  , fullscreen = Just False
  }

instance BearLibConfigString WindowOptions where
  toConfigString WindowOptions{..} =
    let f :: Functor f => BearLibConfigString g => Text -> f g -> f ConfigOption
        f t = fmap (ConfigOption . (t,) . toConfigString)
        mkOptions = map toConfigString $ catMaybes
          [ f "size" size
          , f "cellsize" cellsize
          , f "title" title
          -- todo: work out how filepaths should work
          -- todo: this should probably be done with generics
          , f "icon" icon
          , f "resizeable" resizeable
          , f "fullscreen" fullscreen
          ]
    in
      case mkOptions of
        [] -> mempty
        opts -> LT.fromText "window: " <> mconcat (L.intersperse (LT.singleton ',') $ opts) <> LT.singleton ';'

initWindow :: MonadIO m => WindowOptions -> m ()
initWindow opts = do
  void $ terminalOpen
  void $ terminalSet opts

withWindow :: MonadUnliftIO m => WindowOptions -> m a -> (a -> m b) -> m c -> m b
withWindow opts initialise loop exit = bracket
  (initWindow opts >> initialise)
  (const $ exit >> terminalClose)
  loop

data BlockingMode = Blocking | NotBlocking
  deriving stock (Eq, Ord, Generic, Show, Bounded, Enum)

data Event =
  Keypress Keycode
  | WindowEvent WindowEvent
  deriving stock (Eq, Ord, Generic, Show)

data WindowEvent = Resize | WindowClose
  deriving stock (Eq, Ord, Generic, Show, Bounded)

data Keycode =
  TkA | TkB | TkC | TkD | TkE | TkF | TkG | TkH | TkI | TkJ | TkK | TkL | TkM
  | TkN | TkO | TkP | TkQ | TkR | TkS | TkT | TkU | TkV | TkW | TkX | TkY | TkZ
  | Tk1 | Tk2 | Tk3 | Tk4 | Tk5 | Tk6 | Tk7 | Tk8 | Tk9 | Tk0
  | TkEnter | TkEsc | TkBackspace | TkTab | TkSpace | TkMinus | TkEquals | TkLeftBracket
  | TkRightBracket | TkBackslash | TkSemicolon | TkApostrophe | TkGrave | TkComma | TkPeriod
  | TkSlash | Tk0x39 | TkF1 | TkF2 | TkF3 | TkF4 | TkF5 | TkF6 | TkF7 | TkF8 | TkF9 | TkF10
  | TkF11 | TkF12 | Tk0x46 | Tk0x47 | TkPause | TkInsert | TkHome | TkPageUp | TkDelete | TkPageDown
  | TkRight | TkLeft | TkDown | TkUp | Tk0x53 | TkKPDivide | TkKPMultiply | TkKPMinus | TkKPPlus
  | TkKPEnter | TkKP1 | TkKP2 | TkKP3 | TkKP4 | TkKP5 | TkKP6 | TkKP7 | TkKP8 | TkKP9 | TkKP0
  | TkKPPeriod | TkShift | TkControl | TkAlt
  deriving stock (Eq, Ord, Generic, Show, Bounded, Enum)

safeToEnum :: forall t . (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i =
  if (i >= fromEnum (minBound :: t)) && (i <= fromEnum (maxBound :: t))
    then Just . toEnum $ i
    else Nothing

intToKeycode :: Int -> Keycode
intToKeycode = \case
  0x70 -> TkShift
  0x71 -> TkControl
  0x72 -> TkAlt
  i -> (safeToEnum . (\x -> x - 4) $ i) & \case
    Nothing -> error $ "unknown keycode: " <> show i
    Just w -> w

terminalRead :: MonadIO m => m Event
terminalRead = do
  kc <- terminalReadCode
  return $ case kc of
    225 -> WindowEvent Resize
    224 -> WindowEvent WindowClose
    ikc -> Keypress . intToKeycode $ ikc

handleEvents :: MonadIO m => BlockingMode -> (Event -> m a) -> m [a]
handleEvents bm f = do
  let allEvents :: MonadIO m => m [Event]
      allEvents = go True
        where
          go isEmptySoFar = do
            i <- terminalHasInput
            if i || (bm == Blocking && isEmptySoFar)
            then do
              r <- terminalRead
              (r :) <$> go False
            else
              pure []
  ev <- allEvents
  mapM f ev