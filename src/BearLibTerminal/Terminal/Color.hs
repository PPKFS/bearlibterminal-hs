{-|
Module      : BearLibTerminal.Terminal.Color
Description : Color functions.
License     : MIT
Stability   : experimental
Portability : POSIX

Setting the active foreground/background colors and a couple of helper functions for making colors from (A)RGB quads/triples.
-}

module BearLibTerminal.Terminal.Color
  ( colorFromARGB
  , colorFromRGB
  , terminalColorUInt
  , terminalColorName
  , terminalBkColorUInt
  , terminalBkColorName
  ) where

import Control.Monad.IO.Class
import Data.Text
import BearLibTerminal.Raw
import BearLibTerminal.Terminal.CString
import Data.Bits

-- | Convert a color given as 4 integer values (0-255) into an unsigned integer for use with `terminalColorUInt`. No bounds checking is performed.
colorFromARGB ::
  Integral a
  => a -- ^ alpha channel value.
  -> a -- ^ red channel value.
  -> a -- ^ green channel value.
  -> a -- ^ blue channel value.
  -> Int -- ^ color in 32-bit integer form.
colorFromARGB a r g b = (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

-- | Convert a color given as 3 integer values (0-255) into an unsigned integer for use with `terminalColorUInt`. No bounds checking is performed.
-- The alpha channel is fully opaque (0xFF).
colorFromRGB ::
  Integral a
  => a -- ^ red channel value.
  -> a -- ^ green channel value.
  -> a -- ^ blue channel value.
  -> Int -- ^ color in 32-bit integer form.
colorFromRGB = colorFromARGB 255

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
