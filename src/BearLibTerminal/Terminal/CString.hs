{-|
Module      : BearLibTerminal.Terminal.CString
Description : String functions taking `CString`s.
License     : MIT
Stability   : experimental
Portability : POSIX

Functions that take strings as `CString`s. Unless you are wanting to marshall across the foreign boundary yourself,
you probably don't want to use these (prefer `Data.Text.Text` from `BearLibTerminal` or `String` from `BearLibTerminal.Terminal.String`).
-}

module BearLibTerminal.Terminal.CString
  ( terminalSetCString
  , terminalSetCString_
  , terminalColorNameCString
  , terminalBkColorNameCString

  , terminalPrintCString
  , terminalPrintExtCString

  , terminalMeasureCString
  , terminalMeasureExtCString

  ) where

import BearLibTerminal.Raw
import Control.Monad.IO.Class
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C.String
import Foreign.C.Types (CInt)

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

-- | Measure the size of a string *if it were to be printed to the screen*, given as a `CString`.
-- Wrapper around [@terminal_measure@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureCString ::
  MonadIO m
  => CString -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureCString c = liftIO $ alloca (\dim -> c_terminal_measure_ptr c dim >> peek dim)

-- | Measure the size of a string *if it were to be printed to the screen*, autowrapped in a bounding box, given as a `CString`.
-- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureExtCString ::
  MonadIO m
  => Int -- ^ the width of the bounding box.
  -> Int -- ^ the height of the bounding box.
  -> CString  -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureExtCString w h c = liftIO $ alloca (\dim -> c_terminal_measure_ext_ptr (fromIntegral w) (fromIntegral h) c dim >> peek dim)