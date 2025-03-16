{-|
Module      : BearLibTerminal.Terminal.String
Description : String functions taking `String`s.
License     : MIT
Stability   : experimental
Portability : POSIX

Functions that take strings as `String`s. It is preferable to use the `Data.Text.Text` versions if possible, but these are fine
too.
-}

module BearLibTerminal.Terminal.String
  ( terminalSetString
  , terminalSetString_
  , terminalColorNameString
  , terminalBkColorNameString
  , terminalPrintString
  , terminalPrintString_
  , terminalPrintExtString
  , terminalPrintExtString_

  , terminalMeasureString
  , terminalMeasureExtString

  ) where

import BearLibTerminal.Raw
import BearLibTerminal.Terminal.CString
import Control.Monad (void)
import Control.Monad.IO.Class

-- | Set one or more of the configuration options, given as a `String`.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSetString :: MonadIO m =>
  String -- ^ Configuration string.
  -> m Bool -- ^ whether the configuration was successful.
terminalSetString = stringToCString terminalSetCString

-- | Set one or more of the configuration options, given as a `String`. Ignore if it was successful.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSetString_ :: MonadIO m =>
  String -- ^ Configuration string.
  -> m ()
terminalSetString_ = stringToCString terminalSetCString_

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

-- | Print a string to the screen, given as a `String`. Ignore the returned dimensions.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrintString_ ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> String -- ^ the string to print.
  -> m () -- ^ the `Dimensions` of the string as printed on screen.
terminalPrintString_ x y = void . terminalPrintString x y

-- | Print a string to the screen, given as a `String`, with (optional) auto-wrapping and alignment.
-- Wrapper around [@terminal_print_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#print_ext)
terminalPrintExtString ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> Int -- ^ width of the bounding box for auto-wrapping and alignment.
  -> Int -- ^ height of the bounding box for auto-wrapping and alignment.
  -> Maybe PrintAlignment -- ^ alignment of the string within the bounding box.
  -> String -- ^ the string to print.
  -> m Dimensions -- ^ the `Dimensions` of the string as printed on screen.
terminalPrintExtString x y w h align = stringToCString (terminalPrintExtCString x y w h align)

-- | Print a string to the screen, given as a `String`, with (optional) auto-wrapping and alignment.
-- Ignore the returned dimensions.
-- Wrapper around [@terminal_print_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#print_ext)
terminalPrintExtString_ ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> Int -- ^ width of the bounding box for auto-wrapping and alignment.
  -> Int -- ^ height of the bounding box for auto-wrapping and alignment.
  -> Maybe PrintAlignment -- ^ alignment of the string within the bounding box.
  -> String -- ^ the string to print.
  -> m () -- ^ the `Dimensions` of the string as printed on screen.
terminalPrintExtString_ x y w h align = void . terminalPrintExtString x y w h align


-- | Measure the size of a string *if it were to be printed to the screen*, given as a `String`.
-- Wrapper around [@terminal_measure@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureString ::
  MonadIO m
  => String -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureString = stringToCString terminalMeasureCString

-- | Measure the size of a string *if it were to be printed to the screen*, autowrapped in a bounding box, given as a `String`.
-- Wrapper around [@terminal_measure_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#measure)
terminalMeasureExtString ::
  MonadIO m
  => Int -- ^ the width of the bounding box.
  -> Int -- ^ the height of the bounding box.
  -> String  -- ^ the string to measure the print for.
  -> m Dimensions -- ^ the size of the string if it were printed to the screen.
terminalMeasureExtString w h = stringToCString (terminalMeasureExtCString w h)