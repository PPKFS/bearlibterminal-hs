{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BearMonadTerminal where

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
  , title = Just "BearMonadTerminal"
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
  deriving stock (Eq, Ord, Generic, Show, Bounded, Enum)

safeToEnum :: forall t . (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i =
  if (i >= fromEnum (minBound :: t)) && (i <= fromEnum (maxBound :: t))
    then Just . toEnum $ i
    else Nothing

intToKeycode :: Int -> Keycode
intToKeycode i = (safeToEnum . (\x -> x - 4) $ i) & \case
  Nothing -> error $ "unknown keycode: " <> show i
  Just w -> w

{-
#define TK_A                0x04
#define TK_B                0x05
#define TK_C                0x06
#define TK_D                0x07
#define TK_E                0x08
#define TK_F                0x09
#define TK_G                0x0A
#define TK_H                0x0B
#define TK_I                0x0C
#define TK_J                0x0D
#define TK_K                0x0E
#define TK_L                0x0F
#define TK_M                0x10
#define TK_N                0x11
#define TK_O                0x12
#define TK_P                0x13
#define TK_Q                0x14
#define TK_R                0x15
#define TK_S                0x16
#define TK_T                0x17
#define TK_U                0x18
#define TK_V                0x19
#define TK_W                0x1A
#define TK_X                0x1B
#define TK_Y                0x1C
#define TK_Z                0x1D
#define TK_1                0x1E
#define TK_2                0x1F
#define TK_3                0x20
#define TK_4                0x21
#define TK_5                0x22
#define TK_6                0x23
#define TK_7                0x24
#define TK_8                0x25
#define TK_9                0x26
#define TK_0                0x27
#define TK_RETURN           0x28
#define TK_ENTER            0x28
#define TK_ESCAPE           0x29
#define TK_BACKSPACE        0x2A
#define TK_TAB              0x2B
#define TK_SPACE            0x2C
#define TK_MINUS            0x2D /*  -  */
#define TK_EQUALS           0x2E /*  =  */
#define TK_LBRACKET         0x2F /*  [  */
#define TK_RBRACKET         0x30 /*  ]  */
#define TK_BACKSLASH        0x31 /*  \  */
#define TK_SEMICOLON        0x33 /*  ;  */
#define TK_APOSTROPHE       0x34 /*  '  */
#define TK_GRAVE            0x35 /*  `  */
#define TK_COMMA            0x36 /*  ,  */
#define TK_PERIOD           0x37 /*  .  */
#define TK_SLASH            0x38 /*  /  */
#define TK_F1               0x3A
#define TK_F2               0x3B
#define TK_F3               0x3C
#define TK_F4               0x3D
#define TK_F5               0x3E
#define TK_F6               0x3F
#define TK_F7               0x40
#define TK_F8               0x41
#define TK_F9               0x42
#define TK_F10              0x43
#define TK_F11              0x44
#define TK_F12              0x45
#define TK_PAUSE            0x48 /* Pause/Break */
#define TK_INSERT           0x49
#define TK_HOME             0x4A
#define TK_PAGEUP           0x4B
#define TK_DELETE           0x4C
#define TK_END              0x4D
#define TK_PAGEDOWN         0x4E
#define TK_RIGHT            0x4F /* Right arrow */
#define TK_LEFT             0x50 /* Left arrow */
#define TK_DOWN             0x51 /* Down arrow */
#define TK_UP               0x52 /* Up arrow */
#define TK_KP_DIVIDE        0x54 /* '/' on numpad */
#define TK_KP_MULTIPLY      0x55 /* '*' on numpad */
#define TK_KP_MINUS         0x56 /* '-' on numpad */
#define TK_KP_PLUS          0x57 /* '+' on numpad */
#define TK_KP_ENTER         0x58
#define TK_KP_1             0x59
#define TK_KP_2             0x5A
#define TK_KP_3             0x5B
#define TK_KP_4             0x5C
#define TK_KP_5             0x5D
#define TK_KP_6             0x5E
#define TK_KP_7             0x5F
#define TK_KP_8             0x60
#define TK_KP_9             0x61
#define TK_KP_0             0x62
#define TK_KP_PERIOD        0x63 /* '.' on numpad */
#define TK_SHIFT            0x70
#define TK_CONTROL          0x71
#define TK_ALT              0x72
-}
handleEvents :: MonadIO m => BlockingMode -> (Event -> m a) -> m [a]
handleEvents bm f = do
  let allEvents :: MonadIO m => m [Event]
      allEvents = go True
        where
          go isEmptySoFar = do
            i <- terminalHasInput
            if i || (bm == Blocking && isEmptySoFar)
            then do
              kc <- terminalReadCode
              let r =
                    case kc of
                      225 -> WindowEvent Resize
                      224 -> WindowEvent WindowClose
                      ikc -> Keypress . intToKeycode $ ikc
              (r :) <$> go False
            else
              pure []
  ev <- allEvents
  mapM f ev