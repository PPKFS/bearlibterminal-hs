{-|
Module      : BearLibTerminal.Terminal.Set
Description : Setting configuration options.
License     : MIT
Stability   : experimental
Portability : POSIX

Setting configuration options - e.g. cell size, fonts, window title, etc etc etc. A full list of everything settable is at
http://foo.wyrd.name/en:bearlibterminal:reference:configuration.

There are some helper functions specifically for setting a title, but it is recommended to use a @printf@ style package
to build longer configuration strings.
-}

module BearLibTerminal.Terminal.Set
  ( terminalSet
  , terminalSet_

  , terminalSetTitle
  , terminalSetMany
  ) where

import BearLibTerminal.Raw
import BearLibTerminal.Terminal.CString
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T

-- | Set one or more of the configuration options, given as a `Text`.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSet :: MonadIO m =>
  Text -- ^ Configuration string.
  -> m Bool -- ^ whether the configuration was successful.
terminalSet = textToCString terminalSetCString

-- | Set one or more of the configuration options, given as a `Text`. Ignore if it was successful.
--
-- Wrapper around [@terminal_set@](http://foo.wyrd.name/en:bearlibterminal:reference).
-- More details are available at the BearLibTerminal docs: http://foo.wyrd.name/en:bearlibterminal:reference:configuration.
terminalSet_ ::
  MonadIO m
  => Text -- ^ Configuration string.
  -> m ()
terminalSet_ = textToCString terminalSetCString_

-- | Set the title of the window.
terminalSetTitle :: MonadIO m => Text -> m ()
terminalSetTitle = terminalSet_ . ("window: " <>) . surround "title='" "'"

-- | Set multiple properties at once under a specific super-heading.
terminalSetMany ::
  MonadIO m
  => Text -- ^ the super-heading to set things under (e.g. `window`)
  -> [Text] -- ^ the list of key:value properties to be set.
  -> m ()
terminalSetMany super rest = terminalSet_ $ super <> ": " <> T.intercalate ", " rest