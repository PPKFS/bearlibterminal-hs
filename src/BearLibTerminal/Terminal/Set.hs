module BearLibTerminal.Terminal.Set
  ( terminalSet
  , terminalSetString
  , terminalSetCString
  , terminalSet_
  , terminalSetString_
  , terminalSetCString_

  , terminalSetTitle
  , terminalSetMany
  , titleProperty

  ) where

import BearLibTerminal.Raw
import Control.Monad.IO.Class

import Data.Text (Text)
import BearLibTerminal.Terminal.CString
import BearLibTerminal.Terminal.String
import qualified Data.Text as T
import Data.String (IsString)

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

terminalSetTitle :: MonadIO m => Text -> m ()
terminalSetTitle = terminalSet_ . ("window: " <>) . titleProperty

titleProperty :: IsString a => Semigroup a => a -> a
titleProperty = surround "title='" "'"

terminalSetMany ::
  MonadIO m
  => Text
  -> [Text]
  -> m ()
terminalSetMany super rest = terminalSet_ $ super <> ": " <> T.intercalate ", " rest