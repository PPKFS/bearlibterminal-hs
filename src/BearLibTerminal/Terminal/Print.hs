module BearLibTerminal.Terminal.Print where

import BearLibTerminal.Raw
import Control.Monad.IO.Class

import Data.Text (Text)
import BearLibTerminal.Terminal.CString
import Data.String (IsString)
import Control.Monad (void)

textColor ::
  IsString a
  => Semigroup a
  => a
  -> a
  -> a
textColor col = surround ("[color="<>col<>"]") "[/color]"

textBkColor ::
  IsString a
  => Semigroup a
  => a
  -> a
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