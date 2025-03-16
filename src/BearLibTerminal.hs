{-|
Module      : BearLibTerminal.Keycodes
Description : String functions taking `CString`s.
License     : MIT
Stability   : experimental
Portability : POSIX

  A low-ish level binding to BearLibTerminal. For the most part this is 1-to-1 to the original C/C++ API
    and the raw bindings in `BearLibTerminal.Raw` are almost identical (the only differences being that intcode
    return types are wrapped into Booleans where relevant).
  For functions that expect strings, 3 variants exist:

    - `Text`. This one is strongly preferred because of the performance benefits of `Text` over `String`. These versions
    are available without a suffix - e.g. `terminalSet`, `terminalPrint_`.
    - `String`. Available with a `String` suffix - e.g. `terminalSetString`, `terminalPrintString_`.
    - `CString`. These aren't exported from this module but are available in `BearLibTerminal.Terminal.CString` if you
      do need them. They also do not have `m ()` variants.

  As this library is a low-level wrapper, the original BearLibTerminal documentation is an excellent reference!
  http://foo.wyrd.name/en:bearlibterminal:reference
-}

module BearLibTerminal
  (
  -- * Initialisation and configuration
  -- ** Open
  -- | For initialising a BearLibTerminal instance and opening a window.
    terminalOpen, terminalOpen_
  -- ** Close
  -- | For deinitialising a BearLibTerminal instance and closing the window.
  , terminalClose
  -- ** Set

  -- | For setting configuration options via configuration strings.
  , terminalSet, terminalSetString
  , terminalSet_, terminalSetString_
  -- * Output state

  -- ** Color
  , colorFromARGB
  , colorFromRGB
  -- *** Foreground
  , terminalColorUInt
  , terminalColorName
  , terminalColorNameString
  -- *** Background
  , terminalBkColorUInt
  , terminalBkColorName
  , terminalBkColorNameString

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
  , textColor
  , textBkColor
  , terminalPrint
  , terminalPrint_
  , terminalPrintExt
  , terminalPrintExt_

  , terminalPrintString
  , terminalPrintString_
  , terminalPrintExtString
  , terminalPrintExtString_

  -- ** Put
  , terminalPut
  , terminalPutInt
  , terminalPutExt

  -- ** Measure
  , terminalMeasureText
  , terminalMeasureString
  , terminalMeasureExtText
  , terminalMeasureExtString

  -- ** Refresh
  , terminalRefresh
  -- ** Clear
  , terminalClear
  , terminalClearArea
  , terminalCrop
  -- * Input
  -- ** Events
  , module BearLibTerminal.Keycodes
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
  , terminalKeyState
  -- ** Picking
  , terminalPick
  , terminalPickColor
  -- * Utility
  , terminalDelay
  ) where



import BearLibTerminal.Keycodes
import BearLibTerminal.Raw
import BearLibTerminal.Terminal.CString
import BearLibTerminal.Terminal.Color
import BearLibTerminal.Terminal.Print
import BearLibTerminal.Terminal.Set
import BearLibTerminal.Terminal.String
import Control.Exception
import Control.Monad.IO.Class
import Data.Char (ord)
import Data.Coerce (coerce)
import Data.Text (Text)
import Foreign
import Foreign.C (CUInt)
import GHC.Generics
import qualified Data.Text.Foreign as T
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

-- | Measure the size of a string *if it were to be printed to the screen*, given as a `Text`.
-- Wrapper around [@terminal_measure@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureText ::
  MonadIO m
  => Text -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureText = textToCString terminalMeasureCString

-- | Measure the size of a string *if it were to be printed to the screen*, autowrapped in a bounding box, given as a `Text`.
-- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureExtText ::
  MonadIO m
  => Int -- ^ the width of the bounding box.
  -> Int -- ^ the height of the bounding box.
  -> Text  -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureExtText w h = textToCString (terminalMeasureExtCString w h)

-- | This function queries the current numeric value of a library property; e.g. mouse position or clicks, or key presses/releases.
-- When looking to query for key presses or releases, `terminalKeyState` provides a better interface.
-- Wrapper around [@terminal_state@]. For more information, check the table in the original documentation:
-- http://foo.wyrd.name/en:bearlibterminal:reference:input#event_and_state_constants
terminalState :: MonadIO m => Keycode -> m Int
terminalState = liftIO . fmap fromIntegral . c_terminal_state . fromIntegral

-- | Returns true if there are currently input events in the input queue.
  -- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#has_input)
terminalHasInput ::
  MonadIO m
  => m Bool -- ^ if there are currently input events in the input queue.
terminalHasInput = liftIO $ asBool <$> c_terminal_has_input

-- | Read an event from the input queue and remove it. If there are no pending events in the input queue, block
-- until there is. If you don't want to block until an event comes, check for input with `terminalHasInput` first.
-- This will return a raw integer - prefer `terminalPeek` to be able to use the pattern synonyms of named keycodes.
-- Wrapper around [@terminal_read@](http://foo.wyrd.name/en:bearlibterminal:reference#read).
-- For more information on the input queue, see the original documentation: http://foo.wyrd.name/en:bearlibterminal:reference:input#input_queue
terminalReadCode :: MonadIO m => m Int
terminalReadCode = liftIO $ fromIntegral <$> c_terminal_read

-- | Read an event from the input queue but **do not** remove it. If there are no pending events in the input queue, this will return `Nothing`.
-- This is non-blocking. This will return a raw integer - prefer `terminalPeek` to be able to use the pattern synonyms of named keycodes.
-- Wrapper around [@terminal_peek@](http://foo.wyrd.name/en:bearlibterminal:reference#peek).
-- For more information on the input queue, see the original documentation: http://foo.wyrd.name/en:bearlibterminal:reference:input#input_queue
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
          return (Just t)) `catch` (\(SomeException _) -> return Nothing)
  )
-- | Pause execution for a number of milliseconds.
-- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#delay)
terminalDelay ::
  MonadIO m
  => Int -- ^ amount of time to suspend program execution for, in milliseconds.
  -> m ()
terminalDelay = liftIO . c_terminal_delay . fromIntegral

-- | Read an event from the input queue and remove it. If there are no pending events in the input queue, block
-- until there is. If you don't want to block until an event comes, check for input with `terminalHasInput` first.
-- Wrapper around [@terminal_read@](http://foo.wyrd.name/en:bearlibterminal:reference#read).
-- For more information on the input queue, see the original documentation: http://foo.wyrd.name/en:bearlibterminal:reference:input#input_queue
terminalRead :: MonadIO m => m Keycode
terminalRead = Keycode <$> terminalReadCode

-- | Read an event from the input queue but **do not** remove it. If there are no pending events in the input queue, this will return `Nothing`.
-- This is non-blocking.
-- Wrapper around [@terminal_peek@](http://foo.wyrd.name/en:bearlibterminal:reference#peek).
-- For more information on the input queue, see the original documentation: http://foo.wyrd.name/en:bearlibterminal:reference:input#input_queue
terminalPeek :: MonadIO m => m (Maybe Keycode)
terminalPeek = (\x -> if x == 0 then Nothing else Just (Keycode x)) <$> terminalPeekCode

-- | Get the current status of a key (e.g. pressed or released). This is a more specific version of `terminalState` for when the `Keycode` is
-- a keyboard key. The behaviour will be strange if you use this for e.g. querying the mouse position. Use `terminalState` for that.
-- Wrapper around [@terminal_state@](http://foo.wyrd.name/en:bearlibterminal:reference:input#state).
terminalKeyState :: MonadIO m => Keycode -> m Bool
terminalKeyState = fmap (== 1) . terminalState . coerce