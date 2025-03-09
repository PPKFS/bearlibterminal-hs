module BearLibTerminal.Terminal.Color where

import Control.Monad.IO.Class
import Data.Text
import BearLibTerminal.Raw
import BearLibTerminal.Terminal.CString
import Data.Bits

colorFromARGB ::
  Integral a
  => a
  -> a
  -> a
  -> a
  -> Int
colorFromARGB a r g b = (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | Set the currently selected foreground color to be used by following output functions (e.g. print).
-- Takes a color as a 32-bit unsigned integer in ARGB format.
-- Wrapper around [@terminal_color@](http://foo.wyrd.name/en:bearlibterminal:reference#color).
terminalColorUInt ::
  MonadIO m
  => Int -- ^ the color to set in 32-bit ARGB format.
  -> m ()
terminalColorUInt = liftIO . c_terminal_color_uint . fromIntegral

-- | Set the currently selected foreground color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `Text`.
--
-- Wrapper around [@terminal_color@](http://foo.wyrd.name/en:bearlibterminal:reference#color) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalColorName ::
  MonadIO m
  => Text -- ^ the color name to be selected,in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalColorName = textToCString terminalColorNameCString

-- | Set the currently selected background color to be used by following output functions (e.g. print).
-- Takes a color as a 32-bit unsigned integer in ARGB format.
-- Wrapper around [@terminal_bkcolor@](http://foo.wyrd.name/en:bearlibterminal:reference#bkcolor).
terminalBkColorUInt ::
  MonadIO m
  => Int -- ^ the color to set in 32-bit ARGB format.
  -> m ()
terminalBkColorUInt = liftIO . c_terminal_bkcolor_uint . fromIntegral

-- | Set the currently selected background color to be used by following output functions (e.g. `terminalPrintText`).
-- Takes a color as a name in a variety of formats; see [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name)
-- for a full list.
--
-- Takes the color name as a `Text`.
--
-- Wrapper around [@terminal_bkcolor@](http://foo.wyrd.name/en:bearlibterminal:reference#bkcolor) and
-- [@color_from_name@](http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name).
terminalBkColorName ::
  MonadIO m
  => Text -- ^ the color name to be selected, in @"[brightness]hue"@ format as specified in @color_from_name@.
  -> m ()
terminalBkColorName = textToCString terminalBkColorNameCString
