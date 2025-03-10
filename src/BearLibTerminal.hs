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
    terminalOpen, terminalOpen_
  -- ** Close
  -- | For deinitialising a BearLibTerminal instance and closing the window.
  , terminalClose
  -- ** Set

  -- | For setting configuration options via configuration strings.
  , terminalSet, terminalSetString, terminalSetCString
  , terminalSet_, terminalSetString_, terminalSetCString_
  -- * Output state
  -- ** Color
  -- *** Foreground

  , terminalColorUInt
  , terminalColorName
  -- *** Background
  , terminalBkColorUInt
  , terminalBkColorName
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
  , PrintAlignment(..)
  , Dimensions(..)
  , terminalPrint
  , terminalPrintExt
  , terminalPut
  , terminalPutInt
  , terminalPutExt
  -- ** Measure
  , terminalMeasureText
  , terminalMeasureString
  , terminalMeasureCString
  , terminalMeasureExtText
  , terminalMeasureExtString
  , terminalMeasureExtCString

  -- ** Refresh
  , terminalRefresh
  -- ** Clear
  , terminalClear
  , terminalClearArea
  , terminalCrop
  -- * Input
  -- ** Events
  , Keycode(..)

  , terminalPeek
  , terminalPeekCode
  , terminalHasInput
  , terminalRead
  , terminalReadCode
  -- ** Reading strings
  , terminalReadString
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
import Data.Text (Text)
import Foreign.C (CUInt)
import Foreign.Marshal.Alloc
import qualified Data.Text.Foreign as T
import Foreign
import Control.Exception
import BearLibTerminal.Terminal.Set
import BearLibTerminal.Terminal.Color
import BearLibTerminal.Terminal.Print
import Data.Char (ord)
import BearLibTerminal.Keycodes

maxStringReadSizeInBytes :: Int
maxStringReadSizeInBytes = 8192

-- | Create a new window with the default parameters. Does not display the window until the first call to `terminalRefresh`.
--
-- Wrapper around [@terminal_open@](http://foo.wyrd.name/en:bearlibterminal:reference#open).
terminalOpen :: MonadIO m
  => m Bool -- ^ whether the window creation was successful.
terminalOpen = asBool <$> liftIO c_terminal_open

-- | Create a new window with the default parameters. Does not display the window until the first call to `terminalRefresh`.
-- Ignore the success return value.
-- Wrapper around [@terminal_open@](http://foo.wyrd.name/en:bearlibterminal:reference#open).
terminalOpen_ :: MonadIO m
  => m ()
terminalOpen_ = liftIO $ void c_terminal_open

-- | Close the window and cleanup the BearLibTerminal instance.
--
-- Wrapper around [@terminal_close@](http://foo.wyrd.name/en:bearlibterminal:reference#close).
terminalClose ::
  MonadIO m
  => m ()
terminalClose = liftIO c_terminal_close

-- | Draw a single character (given by its code point) onto the screen on the currently selected layer
-- with the currently selected colors. This takes an `Int` rather than `Char` to avoid possible headaches
-- with mismatching character codes over string conversion. If you know you are wanting to render single
-- *characters* (rather than *code points*), consider using `terminalPrintText` (or similar) instead.
--
-- Wrapper around [@terminal_put@](http://foo.wyrd.name/en:bearlibterminal:reference#put)
terminalPutInt ::
  MonadIO m
  => Int -- ^ x-coordinate to print the character to.
  -> Int -- ^ y-coordinate to print the character to.
  -> Int -- ^ Unicode code point of the character to print.
  -> m ()
terminalPutInt x y c = liftIO $ c_terminal_put (fromIntegral x) (fromIntegral y) (fromIntegral c)

-- | Draw a single character (given by its code point) onto the screen on the currently selected layer
-- with the currently selected colors. This does take `Char` over `Int`, but possible problems with
-- encodings, unicode, all that jazz - you're on your own. If you know your way around string encodings
-- and can advise, please do!
--
-- Wrapper around [@terminal_put@](http://foo.wyrd.name/en:bearlibterminal:reference#put)
terminalPut ::
  MonadIO m
  => Int -- ^ x-coordinate to print the character to.
  -> Int -- ^ y-coordinate to print the character to.
  -> Char -- ^ Unicode code point of the character to print.
  -> m ()
terminalPut x y c = liftIO $ c_terminal_put (fromIntegral x) (fromIntegral y) (fromIntegral $ ord c)

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

-- | Measure the size of a string *if it were to be printed to the screen*, given as a `CString`.
-- Wrapper around [@terminal_measure@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureCString ::
  MonadIO m
  => CString -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureCString c = liftIO $ alloca (\dim -> c_terminal_measure_ptr c dim >> peek dim)

-- | Measure the size of a string *if it were to be printed to the screen*, given as a `String`.
-- Wrapper around [@terminal_measure@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureString ::
  MonadIO m
  => String -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureString = stringToCString terminalMeasureCString

-- | Measure the size of a string *if it were to be printed to the screen*, given as a `Text`.
-- Wrapper around [@terminal_measure@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureText ::
  MonadIO m
  => Text -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureText = textToCString terminalMeasureCString

-- | Measure the size of a string *if it were to be printed to the screen*, autowrapped in a bounding box, given as a `CString`.
-- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureExtCString ::
  MonadIO m
  => Int -- ^ the width of the bounding box.
  -> Int -- ^ the height of the bounding box.
  -> CString  -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureExtCString w h c = liftIO $ alloca (\dim -> c_terminal_measure_ext_ptr (fromIntegral w) (fromIntegral h) c dim >> peek dim)

-- | Measure the size of a string *if it were to be printed to the screen*, autowrapped in a bounding box, given as a `Text`.
-- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureExtText ::
  MonadIO m
  => Int -- ^ the width of the bounding box.
  -> Int -- ^ the height of the bounding box.
  -> Text  -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureExtText w h = textToCString (terminalMeasureExtCString w h)

-- | Measure the size of a string *if it were to be printed to the screen*, autowrapped in a bounding box, given as a `String`.
-- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureExtString ::
  MonadIO m
  => Int -- ^ the width of the bounding box.
  -> Int -- ^ the height of the bounding box.
  -> String  -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureExtString w h = stringToCString (terminalMeasureExtCString w h)

-- TODO TODO TODO
terminalState :: MonadIO m => Int -> m Int
terminalState = liftIO . fmap fromIntegral . c_terminal_state . fromIntegral

-- | Returns true if there are currently input events in the input queue.
  -- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#has_input)
terminalHasInput ::
  MonadIO m
  => m Bool -- ^ if there are currently input events in the input queue.
terminalHasInput = liftIO $ asBool <$> c_terminal_has_input

-- TODO
terminalReadCode :: MonadIO m => m Int
terminalReadCode = liftIO $ fromIntegral <$> c_terminal_read

-- TODO
terminalPeekCode :: MonadIO m => m Int
terminalPeekCode = liftIO $ fromIntegral <$> c_terminal_peek

-- | Read an inputted string and print a text input prompt at the given location.
-- This will block until the string is submitted (with Enter) or cancelled out (with Esc).
-- The string can be up to `maxStringReadSizeInBytes` bytes long.
-- Retyrns nothing if there was an error, or if the user cancelled out.
  -- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#read_str)
terminalReadString ::
  MonadIO m
  => Int -- ^ x-coord of where the user input is displayed.
  -> Int -- ^ y-coord of where the user input is displayed.
  -> Int -- ^ max string length allowed (for the user, not for technical reasons).
  -> m (Maybe Text)
terminalReadString x y m = liftIO $ bracket
  (callocBytes maxStringReadSizeInBytes)
  free
  (\p -> do
    res <- c_read_str (fromIntegral x) (fromIntegral y) p (fromIntegral m)
    if res == -1 || res == 0 then return Nothing else
        (do
          t <- T.peekCStringLen (p, fromIntegral res)
          return (Just t)) `catch` (\(SomeException _) -> do
            print ("failed to decode " <> show res)
            raw <- peekArray (fromIntegral res) p
            print (map castCCharToChar raw, show res) >> return Nothing)
  )
-- | Pause execution for a number of milliseconds.
-- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#delay)
terminalDelay ::
  MonadIO m
  => Int -- ^ amount of time to suspend program execution for, in milliseconds.
  -> m ()
terminalDelay = liftIO . c_terminal_delay . fromIntegral

terminalRead :: MonadIO m => m Keycode
terminalRead = Keycode <$> terminalReadCode

terminalPeek :: MonadIO m => m Keycode
terminalPeek = Keycode <$> terminalPeekCode
