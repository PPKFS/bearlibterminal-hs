{-|
Module      : BearLibTerminal.Terminal.Print
Description : Printing text.
License     : MIT
Stability   : experimental
Portability : POSIX

Functions for printing text to the screen (given as `Data.Text.Text`), and a couple of helper functions for wrapping
strings with inline color strings.
-}

module BearLibTerminal.Terminal.Print
  ( textColor
  , textBkColor
  , terminalPrint
  , terminalPrint_
  , terminalPrintExt
  , terminalPrintExt_
  ) where

import BearLibTerminal.Raw
import Control.Monad.IO.Class

import Data.Text (Text)
import BearLibTerminal.Terminal.CString
import Data.String (IsString)
import Control.Monad (void)

-- | Wrap a string (in any `IsString` format) with color formatting tags.
textColor ::
  IsString a
  => Semigroup a
  => a -- ^ the color, from the list of valid color identifiers - http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name.
  -> a -- ^ the text to wrap.
  -> a
textColor col = surround ("[color="<>col<>"]") "[/color]"

-- | Wrap a string (in any `IsString` format) with background color formatting tags.
textBkColor ::
  IsString a
  => Semigroup a
  => a -- ^ the background color, from the list of valid color identifiers - http://foo.wyrd.name/en:bearlibterminal:reference#color_from_name.
  -> a -- ^ the text to wrap.
  -> a
textBkColor col = surround ("[bkcolor="<>col<>"]") "[/bkcolor]"

-- | Print a string to the screen, given as a `Text`.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrint ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> Text -- ^ the string to print.
  -> m Dimensions -- ^ the `Dimensions` of the string as printed on screen.
terminalPrint x y = textToCString (terminalPrintCString x y)

-- | Print a string to the screen, given as a `Text`, with (optional) auto-wrapping and alignment.
-- Wrapper around [@terminal_print_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#print_ext)
terminalPrintExt ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> Int -- ^ width of the bounding box for auto-wrapping and alignment.
  -> Int -- ^ height of the bounding box for auto-wrapping and alignment.
  -> Maybe PrintAlignment -- ^ alignment of the string within the bounding box.
  -> Text -- ^ the string to print.
  -> m Dimensions -- ^ the `Dimensions` of the string as printed on screen.
terminalPrintExt x y w h align = textToCString (terminalPrintExtCString x y w h align)

-- | Print a string to the screen, given as a `Text`. Ignore the dimensions of the printed string.
--
-- Wrapper around [@terminal_print@](http://foo.wyrd.name/en:bearlibterminal:reference#print).
terminalPrint_ ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> Text -- ^ the string to print.
  -> m ()
terminalPrint_ x y t = void $ terminalPrint x y t

-- | Print a string to the screen, given as a `Text`, with (optional) auto-wrapping and alignment.
-- gnore the dimensions of the printed string.
-- Wrapper around [@terminal_print_ext@](http://foo.wyrd.name/en:bearlibterminal:reference#print_ext)
terminalPrintExt_ ::
  MonadIO m
  => Int  -- ^ x-coordinate to start printing the string at.
  -> Int -- ^ y-coordinate to start printing the string at.
  -> Int -- ^ width of the bounding box for auto-wrapping and alignment.
  -> Int -- ^ height of the bounding box for auto-wrapping and alignment.
  -> Maybe PrintAlignment -- ^ alignment of the string within the bounding box.
  -> Text -- ^ the string to print.
  -> m ()
terminalPrintExt_ x y w h align t = void $ terminalPrintExt x y w h align t