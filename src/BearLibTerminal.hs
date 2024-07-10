{-# LANGUAGE ScopedTypeVariables #-}

module BearLibTerminal
  (

  -- | A low-ish level binding to BearLibTerminal. For the most part this is 1-to-1 to the original C/C++ API
  -- and the raw bindings in `BearLibTerminal.Raw` are almost identical (the only differences being that intcode
  -- return types are wrapped into Booleans where relevant).
  --
  -- For functions that expect strings, 4 variants exist:
  --
  -- - `Text` (strongly preferred)
  -- - `String`
  -- - `ByteString`
  -- - `CString` (avoid unless you want to do memory management and marshalling yourself)

  -- * Initialisation and configuration
  -- ** Open
  -- | For initialising a BearLibTerminal instance and opening a window.
    terminalOpen
  -- ** Close
  -- | For deinitialising a BearLibTerminal instance and closing the window.
  , terminalClose
  -- ** Set

  -- | For setting configuration options via configuration strings.
  , terminalSetText
  , terminalSetString
  , terminalSetBS
  , terminalSetCString
  -- * Output state

  -- | For setting stateful output options (cell colors, layers, and composition).
  -- Yes, it would be great to have this in a more declarative style but that's out of the scope of this library. :)
  --
  -- ** Color
  -- *** Foreground
  , terminalColorUInt
  , terminalColorNameText
  , terminalColorNameString
  , terminalColorNameBS
  , terminalColorNameCString
  -- *** Background
  , terminalBkColorUInt
  , terminalBkColorNameText
  , terminalBkColorNameString
  , terminalBkColorNameBS
  , terminalBkColorNameCString
  -- ** Composition
  , TerminalCompositionMode(..)
  , terminalComposition
  -- ** Layer
  , terminalLayer
  -- * Output
  -- ** Print

  -- | These wrap @terminal_print@ and @terminal_print_ext@ in various string flavours.
  -- These *do* support the inline formatting options that BearLibTerminal itself supports, such as using @[color=red]@,
  -- pixel offsets, different fonts, and more. For more information check the BearLibTerminal documentation for
  -- [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
  , terminalPrintText
  , terminalPrintString
  , terminalPrintBS
  , terminalPrintCString
  , terminalPrintExtText
  , terminalPrintExtString
  , terminalPrintExtBS
  , terminalPrintExtCString
  , terminalPut
  , terminalPutExt
  -- ** Measure
  , terminalMeasureText
  , terminalMeasureString
  , terminalMeasureBS
  , terminalMeasureCString
  , terminalMeasureExtText
  , terminalMeasureExtString
  , terminalMeasureExtBS
  , terminalMeasureExtCString

  -- ** Refresh
  , terminalRefresh
  -- ** Clear
  , terminalClear
  , terminalClearArea
  , terminalCrop
  -- * Input
  -- ** Events
  , Event(..)
  , Keycode(..)
  , WindowEvent(..)
  , intToKeycode

  , terminalPeek
  , terminalPeekCode
  , terminalHasInput
  , terminalRead
  , terminalReadCode
  -- ** Reading strings
  , terminalReadStr
  -- ** State
  , terminalState
  -- ** Picking
  , terminalPick
  , terminalPickColor

  -- * Utility
  , terminalDelay
  ) where

import BearLibTerminal.Raw
import GHC.Generics
import Data.Function ((&))
import Control.Monad.IO.Class

import Foreign.C.String
import Data.ByteString ( ByteString )
import Data.Text (Text)
import Foreign.C (CUInt)
import Foreign.Marshal.Alloc
import qualified Data.Text.Foreign as T
import Foreign

-- | Create a new window with the default parameters. Does not display the window until the first call to `terminalRefresh`.
--
-- Wrapper around [@terminal_open@](http://foo.wyrd.name/en:bearlibterminal:reference#open).
terminalOpen :: MonadIO m
  => m Bool -- ^ whether the window creation was successful.
terminalOpen = asBool <$> liftIO c_terminal_open

-- | Close the window and cleanup the BearLibTerminal instance.
--
-- Wrapper around [@terminal_close@](http://foo.wyrd.name/en:bearlibterminal:reference#close).
terminalClose :: MonadIO m => m ()
terminalClose = liftIO c_terminal_close

-- | Set one or more of the configuration options, given as a `String`.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSetString :: MonadIO m =>
  String -- ^ Configuration string.
  -> m Bool -- ^ whether the configuration was successful.
terminalSetString = stringToCString terminalSetCString

-- | Set one or more of the configuration options, given as a `CString`.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSetCString :: MonadIO m =>
  CString -- ^ Configuration string.
  -> m Bool -- ^ whether the configuration was successful.
terminalSetCString = liftIO . (fmap asBool . c_terminal_set)

-- | Set one or more of the configuration options, given as a `ByteString`.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSetBS :: MonadIO m =>
  ByteString -- ^ Configuration string.
  -> m Bool -- ^ whether the configuration was successful.
terminalSetBS = bsToCString terminalSetCString

-- | Set one or more of the configuration options, given as a `Text`.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSetText :: MonadIO m =>
  Text -- ^ Configuration string.
  -> m Bool -- ^ whether the configuration was successful.
terminalSetText = textToCString terminalSetCString

terminalColorUInt :: MonadIO m => CUInt -> m ()
terminalColorUInt = liftIO . c_terminal_color_uint

terminalColorNameCString :: MonadIO m => CString -> m ()
terminalColorNameCString = liftIO . c_terminal_color_from_name

terminalColorNameString :: MonadIO m => String -> m ()
terminalColorNameString = stringToCString terminalColorNameCString

terminalColorNameText :: MonadIO m => Text -> m ()
terminalColorNameText = textToCString terminalColorNameCString

terminalColorNameBS :: MonadIO m => ByteString -> m ()
terminalColorNameBS = bsToCString terminalColorNameCString

terminalBkColorUInt :: MonadIO m => CUInt -> m ()
terminalBkColorUInt = liftIO . c_terminal_bkcolor_uint

terminalBkColorNameString :: MonadIO m => String -> m ()
terminalBkColorNameString = stringToCString terminalColorNameCString

terminalBkColorNameCString :: MonadIO m => CString -> m ()
terminalBkColorNameCString = liftIO . c_terminal_bkcolor_from_name

terminalBkColorNameText :: MonadIO m => Text -> m ()
terminalBkColorNameText = textToCString terminalBkColorNameCString

terminalBkColorNameBS :: MonadIO m => ByteString -> m ()
terminalBkColorNameBS = bsToCString terminalBkColorNameCString

terminalPut :: MonadIO m => Int -> Int -> Int -> m ()
terminalPut x y c = liftIO $ c_terminal_put (fromIntegral x) (fromIntegral y) (fromIntegral c)

data TerminalCompositionMode = CompositionOn | CompositionOff
  deriving stock (Eq, Ord, Show, Generic, Read, Enum, Bounded)

terminalComposition :: MonadIO m => TerminalCompositionMode -> m ()
terminalComposition = liftIO . c_terminal_composition .
  (\case
    CompositionOff -> 0
    CompositionOn -> 1
  )

terminalLayer :: MonadIO m => Int -> m ()
terminalLayer = liftIO . c_terminal_layer . fromIntegral

terminalClear :: MonadIO m => m ()
terminalClear = liftIO c_terminal_clear

terminalClearArea :: MonadIO m => Int -> Int -> Int -> Int -> m ()
terminalClearArea x y w h = liftIO (c_terminal_clear_area (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))

terminalCrop :: MonadIO m => Int -> Int -> Int -> Int -> m ()
terminalCrop x y w h = liftIO (c_terminal_crop (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))

terminalRefresh :: MonadIO m => m ()
terminalRefresh = liftIO c_terminal_refresh

terminalPick :: MonadIO m => Int -> Int -> Int -> m Int
terminalPick x y i = liftIO $ fromIntegral <$> c_terminal_pick (fromIntegral x) (fromIntegral y) (fromIntegral i)

terminalPickColor :: MonadIO m => Int -> Int -> Int -> m Integer
terminalPickColor x y i = liftIO $ fromIntegral <$> c_terminal_pick_color (fromIntegral x) (fromIntegral y) (fromIntegral i)

terminalPickBkColor :: MonadIO m => Int -> Int -> m Integer
terminalPickBkColor x y = liftIO $ fromIntegral <$> c_terminal_pick_bkcolor (fromIntegral x) (fromIntegral y)

terminalPutExt :: MonadIO m => Int -> Int -> Int -> Int -> Int -> Maybe (Integer, Integer, Integer, Integer) -> m ()
terminalPutExt x y dx dy code mbColors = liftIO $ colorsToArr $ c_terminal_put_ext (fromIntegral x) (fromIntegral y) (fromIntegral dx) (fromIntegral dy) (fromIntegral code)
  where
    colorsToArr :: (Ptr CUInt -> IO a) -> IO a
    colorsToArr f = case mbColors of
      Nothing -> f nullPtr
      Just (tl, bl, br, tr) -> withArray (map fromIntegral [tl, bl, br, tr]) f

-- | Print a string to the screen, given as a `CString`.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrintCString :: MonadIO m => Int -> Int -> CString -> m Dimensions
terminalPrintCString x y c = liftIO $ alloca (\dim -> c_terminal_print_ptr (fromIntegral x) (fromIntegral y) c dim >> peek dim)

-- | Print a string to the screen, given as a `ByteString`.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrintBS :: MonadIO m => Int -> Int -> ByteString -> m Dimensions
terminalPrintBS x y = bsToCString (terminalPrintCString x y)

-- | Print a string to the screen, given as a `String`.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrintString :: MonadIO m => Int -> Int -> String -> m Dimensions
terminalPrintString x y = stringToCString (terminalPrintCString x y)

-- | Print a string to the screen, given as a `Text`.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrintText :: MonadIO m => Int -> Int -> Text -> m Dimensions
terminalPrintText x y = textToCString (terminalPrintCString x y)

terminalPrintExtCString :: MonadIO m => Int -> Int -> Int -> Int -> Int -> CString -> m Dimensions
terminalPrintExtCString x y w h align c = liftIO $ alloca (\dim -> c_terminal_print_ext_ptr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) (fromIntegral align) c dim >> peek dim)

terminalPrintExtBS :: MonadIO m => Int -> Int -> Int -> Int -> Int -> ByteString -> m Dimensions
terminalPrintExtBS x y w h align = bsToCString (terminalPrintExtCString x y w h align)

terminalPrintExtString :: MonadIO m => Int -> Int -> Int -> Int -> Int -> String -> m Dimensions
terminalPrintExtString x y w h align = stringToCString (terminalPrintExtCString x y w h align)

terminalPrintExtText :: MonadIO m => Int -> Int -> Int -> Int -> Int -> Text -> m Dimensions
terminalPrintExtText x y w h align = textToCString (terminalPrintExtCString x y w h align)

terminalMeasureCString :: MonadIO m => CString -> m Dimensions
terminalMeasureCString c = liftIO $ alloca (\dim -> c_terminal_measure_ptr c dim >> peek dim)

terminalMeasureBS :: MonadIO m => ByteString -> m Dimensions
terminalMeasureBS = bsToCString terminalMeasureCString

terminalMeasureString :: MonadIO m => String -> m Dimensions
terminalMeasureString = stringToCString terminalMeasureCString

terminalMeasureText :: MonadIO m => Text -> m Dimensions
terminalMeasureText = textToCString terminalMeasureCString

terminalMeasureExtCString :: MonadIO m => Int -> Int -> CString -> m Dimensions
terminalMeasureExtCString w h c = liftIO $ alloca (\dim -> c_terminal_measure_ext_ptr (fromIntegral w) (fromIntegral h) c dim >> peek dim)

terminalMeasureExtBS :: MonadIO m => Int -> Int -> ByteString -> m Dimensions
terminalMeasureExtBS w h = bsToCString (terminalMeasureExtCString w h)

terminalMeasureExtText :: MonadIO m => Int -> Int -> Text -> m Dimensions
terminalMeasureExtText w h = textToCString (terminalMeasureExtCString w h)

terminalMeasureExtString :: MonadIO m => Int -> Int -> String -> m Dimensions
terminalMeasureExtString w h = stringToCString (terminalMeasureExtCString w h)

terminalState :: MonadIO m => Int -> m Int
terminalState = liftIO . fmap fromIntegral . c_terminal_state . fromIntegral

terminalHasInput :: MonadIO m => m Bool
terminalHasInput = liftIO $ asBool <$> c_terminal_has_input

terminalReadCode :: MonadIO m => m Int
terminalReadCode = liftIO $ fromIntegral <$> c_terminal_read

terminalPeekCode :: MonadIO m => m Int
terminalPeekCode = liftIO $ fromIntegral <$> c_terminal_peek

terminalReadStr :: MonadIO m => Int -> Int -> Int -> m (Maybe Text)
terminalReadStr x y m = liftIO $ alloca (\c -> c_read_str (fromIntegral x) (fromIntegral y) c (fromIntegral m) >>=
  \res -> if res == -1 then return Nothing else Just <$> T.peekCStringLen (c, fromIntegral res))

terminalDelay :: MonadIO m => Int -> m ()
terminalDelay = liftIO . c_terminal_delay . fromIntegral

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

intToKeycode :: Int -> Keycode
intToKeycode = \case
  0x70 -> TkShift
  0x71 -> TkControl
  0x72 -> TkAlt
  i -> (safeToEnum . (\x -> x - 4) $ i) & \case
    Nothing -> error $ "unknown keycode: " <> show i
    Just w -> w

terminalRead :: MonadIO m => m Event
terminalRead = codeToEvent <$> terminalReadCode

terminalPeek :: MonadIO m => m Event
terminalPeek = codeToEvent <$> terminalPeekCode

codeToEvent :: Int -> Event
codeToEvent = \case
    225 -> WindowEvent Resize
    224 -> WindowEvent WindowClose
    ikc -> Keypress . intToKeycode $ ikc
