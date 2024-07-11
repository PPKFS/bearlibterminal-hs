{-# LANGUAGE ScopedTypeVariables #-}

module BearLibTerminal
  (

  {- | A low-ish level binding to BearLibTerminal. For the most part this is 1-to-1 to the original C/C++ API
    and the raw bindings in `BearLibTerminal.Raw` are almost identical (the only differences being that intcode
    return types are wrapped into Booleans where relevant).
  For functions that expect strings, 4 variants exist:

    - `Text` (strongly preferred)
    - `String`
    - `ByteString`
    - `CString` (avoid unless you want to do memory management and marshalling yourself)

  As this library is a low-level wrapper, the original BearLibTerminal documentation is an excellent reference!
  http://foo.wyrd.name/en:bearlibterminal:reference
  -}
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

-- | Set the currently selected foreground color to be used by following output functions (e.g. print).
-- Takes a color as a 32-bit unsigned integer in ARGB format.
-- Wrapper around [@terminal_color@](http://foo.wyrd.name/en:bearlibterminal:reference#color).
terminalColorUInt ::
  MonadIO m
  => CUInt -- ^ the color to set in 32-bit ARGB format.
  -> m ()
terminalColorUInt = liftIO . c_terminal_color_uint

-- | Set the currently selected foreground color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `CString`.
--
-- Wrapper around [@terminal_color@](http://foo.wyrd.name/en:bearlibterminal:reference#color) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalColorNameCString ::
  MonadIO m
  => CString  -- ^ the color name to be selected, in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalColorNameCString = liftIO . c_terminal_color_from_name

-- | Set the currently selected foreground color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `String`.
--
-- Wrapper around [@terminal_color@](http://foo.wyrd.name/en:bearlibterminal:reference#color) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalColorNameString ::
  MonadIO m
  => String  -- ^ the color name to be selected, in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalColorNameString = stringToCString terminalColorNameCString

-- | Set the currently selected foreground color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `Text`.
--
-- Wrapper around [@terminal_color@](http://foo.wyrd.name/en:bearlibterminal:reference#color) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalColorNameText ::
  MonadIO m
  => Text -- ^ the color name to be selected,in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalColorNameText = textToCString terminalColorNameCString

-- | Set the currently selected foreground color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `ByteString`.
--
-- Wrapper around [@terminal_color@](http://foo.wyrd.name/en:bearlibterminal:reference#color) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalColorNameBS :: MonadIO m =>
  ByteString -- ^ the color name to be selected,in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalColorNameBS = bsToCString terminalColorNameCString

-- | Set the currently selected background color to be used by following output functions (e.g. print).
-- Takes a color as a 32-bit unsigned integer in ARGB format.
-- Wrapper around [@terminal_bkcolor@](http://foo.wyrd.name/en:bearlibterminal:reference#bkcolor).
terminalBkColorUInt ::
  MonadIO m
  => CUInt -- ^ the color to set in 32-bit ARGB format.
  -> m ()
terminalBkColorUInt = liftIO . c_terminal_bkcolor_uint

-- | Set the currently selected foreground color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `CString`.
--
-- Wrapper around [@terminal_bkcolor@](http://foo.wyrd.name/en:bearlibterminal:reference#bkcolor) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalBkColorNameCString ::
  MonadIO m
  => CString  -- ^ the color name to be selected, in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalBkColorNameCString = liftIO . c_terminal_bkcolor_from_name

-- | Set the currently selected background color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `String`.
--
-- Wrapper around [@terminal_bkcolor@](http://foo.wyrd.name/en:bearlibterminal:reference#bkcolor) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalBkColorNameString ::
  MonadIO m
  => String  -- ^ the color name to be selected, in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalBkColorNameString = stringToCString terminalBkColorNameCString

-- | Set the currently selected background color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `Text`.
--
-- Wrapper around [@terminal_bkcolor@](http://foo.wyrd.name/en:bearlibterminal:reference#bkcolor) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalBkColorNameText ::
  MonadIO m
  => Text -- ^ the color name to be selected, in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalBkColorNameText = textToCString terminalBkColorNameCString

-- | Set the currently selected background color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `ByteString`.
--
-- Wrapper around [@terminal_bkcolor@](http://foo.wyrd.name/en:bearlibterminal:reference#bkcolor) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalBkColorNameBS :: MonadIO m =>
  ByteString -- ^ the color name to be selected, in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalBkColorNameBS = bsToCString terminalBkColorNameCString

-- | Draw a single character (given by its code point) onto the screen on the currently selected layer
-- with the currently selected colors. This takes an `Int` rather than `Char` to avoid possible headaches
-- with mismatching character codes over string conversion. If you know you are wanting to render single
-- *characters* (rather than *code points*), consider using `terminalPrintText` (or similar) instead.
--
-- Wrapper around [@terminal_put@](http://foo.wyrd.name/en:bearlibterminal:reference#put)
terminalPut ::
  MonadIO m
  => Int -- ^ x-coordinate to print the character to.
  -> Int -- ^ y-coordinate to print the character to.
  -> Int -- ^ Unicode code point of the character to print.
  -> m ()
terminalPut x y c = liftIO $ c_terminal_put (fromIntegral x) (fromIntegral y) (fromIntegral c)

-- | Data type to represent whether cell composition should be turned on or off.
data TerminalCompositionMode =
  CompositionOn -- ^ Turn cell composition on; i.e. repeated prints to the same location on the same layer will stack the tiles.
  | CompositionOff-- ^ Turn cell composition off; i.e. repeated prints to the same location on the same layer will overwrite the existing tiles.
  deriving stock (Eq, Ord, Show, Generic, Read, Enum, Bounded)

-- | Enable or disable terminal composition.
--
-- Wrapper around [@terminal_composition@](http://foo.wyrd.name/en:bearlibterminal:reference#composition)
terminalComposition ::
  MonadIO m
  => TerminalCompositionMode -- ^ composition mode to set.
  -> m ()
terminalComposition = liftIO . c_terminal_composition .
  (\case
    CompositionOff -> 0
    CompositionOn -> 1
  )

-- | Set the currently selected layer to be used by following output functions (e.g. `terminalPrintText`).
--
-- Wrapper around [@terminal_layer@](http://foo.wyrd.name/en:bearlibterminal:reference#layer).
terminalLayer ::
  MonadIO m
  => Int -- ^ layer to select (0-255 inclusive).
  -> m ()
terminalLayer = liftIO . c_terminal_layer . fromIntegral

-- | Clear the entire screen (all layers) with the currently selected background color (`terminalBkColorUInt` and family).
--
-- Wrapper around [@terminal_clear@](http://foo.wyrd.name/en:bearlibterminal:reference#clear).
terminalClear :: MonadIO m => m ()
terminalClear = liftIO c_terminal_clear

-- | Clear a rectangular area on the currently selected layer. This only sets the background color
-- on layer 0.
--
-- Wrapper around [@terminal_clear_area@](http://foo.wyrd.name/en:bearlibterminal:reference#clear_area).
terminalClearArea ::
  MonadIO m
  => Int -- ^ x-coordinate of the top left of the rectangular area.
  -> Int -- ^ y-coordinate of the top left of the rectangular area.
  -> Int -- ^ width of the rectangular area.
  -> Int  -- ^ height of the rectangular area.
  -> m ()
terminalClearArea x y w h = liftIO (c_terminal_clear_area (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))

-- | Set a cropping area for the currently selected layer. Anything printed outside of this region
-- will be ignored. Set the width or height to zero or use `terminalClear` to reset this.
--
-- Wrapper around [@terminal_crop@](http://foo.wyrd.name/en:bearlibterminal:reference#crop).
terminalCrop ::
  MonadIO m
  => Int -- ^ x-coordinate of the top left of the rectangular area.
  -> Int -- ^ y-coordinate of the top left of the rectangular area.
  -> Int -- ^ width of the rectangular area.
  -> Int  -- ^ height of the rectangular area.
  -> m ()
terminalCrop x y w h = liftIO (c_terminal_crop (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h))

-- | Commit the scene to be rendered and redraw the window. This needs to be called once after `terminalOpen`
-- before the window will be displayed.
--
-- Wrapper around [@terminal_refresh@](http://foo.wyrd.name/en:bearlibterminal:reference#crop).
terminalRefresh :: MonadIO m => m ()
terminalRefresh = liftIO c_terminal_refresh

-- | Return the code of the tile in the specified cell on the current layer at the current index
-- in the tile stack.
--
-- Wrapper around [@terminal_pick@](http://foo.wyrd.name/en:bearlibterminal:reference#pick).
terminalPick ::
  MonadIO m
  => Int -- ^ x-coordinate of the tile to be queried.
  -> Int -- ^ y-coordinate of the tile to be queried.
  -> Int  -- ^ index of the symbol in the cell to be returned.
  -> m (Maybe Int) -- ^ `Just codepoint` if the symbol exists and `Nothing` otherwise.
terminalPick x y i = liftIO $ (\x' -> if x' == 0 then Nothing else Just $ fromIntegral x') <$> c_terminal_pick (fromIntegral x) (fromIntegral y) (fromIntegral i)

-- | Return the foreground color of the tile in the specified cell on the current layer at the current index
-- in the tile stack.
--
-- Wrapper around [@terminal_pick_color@](http://foo.wyrd.name/en:bearlibterminal:reference#pick_color).
terminalPickColor ::
  MonadIO m
  => Int -- ^ x-coordinate of the tile to be queried.
  -> Int -- ^ y-coordinate of the tile to be queried.
  -> Int  -- ^ index of the symbol in the cell to be returned.
  -> m Integer -- ^ Color of the foreground if the symbol exists.
terminalPickColor x y i = liftIO $ fromIntegral <$> c_terminal_pick_color (fromIntegral x) (fromIntegral y) (fromIntegral i)

-- | Return the background color of the tile in the specified cell.
--
-- Wrapper around [@terminal_pick_bkcolor@](http://foo.wyrd.name/en:bearlibterminal:reference#pick_bkcolor).
terminalPickBkColor ::
  MonadIO m
  => Int -- ^ x-coordinate of the tile to be queried.
  -> Int -- ^ y-coordinate of the tile to be queried.
  -> m Integer -- ^ Color of the background of the cell.
terminalPickBkColor x y = liftIO $ fromIntegral <$> c_terminal_pick_bkcolor (fromIntegral x) (fromIntegral y)

-- | Draw a single character (given by its code point) onto the screen on the currently selected layer
-- with the currently selected colors and with additional options.
-- This takes an `Int` rather than `Char` to avoid possible headaches
-- with mismatching character codes over string conversion. If you know you are wanting to render single
-- *characters* (rather than *code points*), consider using `terminalPrintText` (or similar) instead.
--
-- Wrapper around [@terminal_put@](http://foo.wyrd.name/en:bearlibterminal:reference#put_ext)
terminalPutExt ::
  MonadIO m
  => Int -- ^ x-coordinate to print the character to.
  -> Int -- ^ y-coordinate to print the character to.
  -> Int -- ^ Internal x offset relative to the normal position of a tile in the cell, in pixels.
  -> Int -- ^ Internal y offset relative to the normal position of a tile in the cell, in pixels.
  -> Int -- ^ Unicode code point of the character to print.
  -> Maybe (Integer, Integer, Integer, Integer) -- ^ individual colours specified per-corner for this
  -- tile (in order: top-left, bottom-left, bottom-right, top-right)
  -> m ()
terminalPutExt x y dx dy code mbColors = liftIO $ colorsToArr $ c_terminal_put_ext (fromIntegral x) (fromIntegral y) (fromIntegral dx) (fromIntegral dy) (fromIntegral code)
  where
    colorsToArr :: (Ptr CUInt -> IO a) -> IO a
    colorsToArr f = case mbColors of
      Nothing -> f nullPtr
      Just (tl, bl, br, tr) -> withArray (map fromIntegral [tl, bl, br, tr]) f

-- | Print a string to the screen, given as a `CString`.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrintCString ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> CString -- ^ the string to print.
  -> m Dimensions -- ^ the `Dimensions` of the string as printed on screen.
terminalPrintCString x y c = liftIO $ alloca (\dim -> c_terminal_print_ptr (fromIntegral x) (fromIntegral y) c dim >> peek dim)

-- | Print a string to the screen, given as a `ByteString`.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrintBS ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> ByteString -- ^ the string to print.
  -> m Dimensions -- ^ the `Dimensions` of the string as printed on screen.
terminalPrintBS x y = bsToCString (terminalPrintCString x y)

-- | Print a string to the screen, given as a `String`.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrintString ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> String -- ^ the string to print.
  -> m Dimensions -- ^ the `Dimensions` of the string as printed on screen.
terminalPrintString x y = stringToCString (terminalPrintCString x y)

-- | Print a string to the screen, given as a `Text`.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrintText ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> Text -- ^ the string to print.
  -> m Dimensions -- ^ the `Dimensions` of the string as printed on screen.
terminalPrintText x y = textToCString (terminalPrintCString x y)

-- | Print a string to the screen, given as a `CString`, with some additional options.
-- TODO: these alignment issues should really be their own type rather than an int.
-- Wrapper around [@terminal_print_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#print_ext)
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
