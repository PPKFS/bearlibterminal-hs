module BearLibTerminal.Terminal.CString
  ( terminalSetCString
  , terminalSetCString_
  , terminalColorNameCString
  , terminalBkColorNameCString

  , terminalPrintCString
  , terminalPrintExtCString

  ) where

import BearLibTerminal.Raw
import Control.Monad.IO.Class
import Foreign.C.String
import Foreign
import Foreign.C.Types (CInt)
import Data.Maybe (fromMaybe)
import Data.Function ((&))

-- | Set one or more of the configuration options, given as a `CString`.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSetCString :: MonadIO m =>
  CString -- ^ Configuration string.
  -> m Bool -- ^ whether the configuration was successful.
terminalSetCString = liftIO . (fmap asBool . c_terminal_set)

-- | Set one or more of the configuration options, given as a `CString`. Ignore if it was successful.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSetCString_ :: MonadIO m =>
  CString -- ^ Configuration string.
  -> m ()
terminalSetCString_ = liftIO . (void . c_terminal_set)

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
-- Takes the color name as a `CString`.
--
-- Wrapper around [@terminal_bkcolor@](http://foo.wyrd.name/en:bearlibterminal:reference#bkcolor) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalBkColorNameCString ::
  MonadIO m
  => CString  -- ^ the color name to be selected, in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalBkColorNameCString = liftIO . c_terminal_bkcolor_from_name

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

-- | Print a string to the screen, given as a `CString`, with (optional) auto-wrapping and alignment.
-- Wrapper around [@terminal_print_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#print_ext)
terminalPrintExtCString ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> Int -- ^ width of the bounding box for auto-wrapping and alignment.
  -> Int -- ^ height of the bounding box for auto-wrapping and alignment.
  -> Maybe PrintAlignment -- ^ alignment of the string within the bounding box.
  -> CString -- ^ the string to print.
  -> m Dimensions -- ^ the `Dimensions` of the string as printed on screen.
terminalPrintExtCString x y w h mbAlign c =
  let align :: CInt
      align = fromMaybe AlignDefault mbAlign & \case
        AlignDefault -> 0
        AlignLeft -> 1
        AlignRight -> 2
        AlignCenter -> 3
        AlignTop -> 4
        AlignBottom -> 8
        AlignMiddle -> 12
  in
  liftIO $ alloca (\dim -> c_terminal_print_ext_ptr (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) align c dim >> peek dim)