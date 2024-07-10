{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module BearLibTerminal.Raw where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Storable
import Control.Monad.IO.Class
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Foreign as TF

data Dimensions = Dimensions
  { width :: Int
  , height :: Int
  } deriving stock (Show)

instance Storable Dimensions where
  sizeOf _ = 8
  alignment _ = 4
  poke p Dimensions{..} = do
    pokeByteOff p 0 width
    pokeByteOff p 4 height
  peek p = do
    width <- peekByteOff p 0
    height <- peekByteOff p 4
    return $ Dimensions width height

asBool :: CInt -> Bool
asBool = (== 1)

foreign import capi safe "BearLibTerminal.h terminal_open" c_terminal_open :: IO CInt

foreign import capi safe "BearLibTerminal.h terminal_close" c_terminal_close :: IO ()

foreign import capi safe "BearLibTerminal.h terminal_set" c_terminal_set :: CString -> IO CInt

bsToCString :: MonadIO m => (CString -> IO a) -> ByteString -> m a
bsToCString f = liftIO . flip BS.useAsCString f

textToCString :: MonadIO m => (CString -> IO a) -> Text -> m a
textToCString f = liftIO . flip TF.withCString f

stringToCString :: MonadIO m => (CString -> IO a) -> String -> m a
stringToCString f = liftIO . flip withCString f

foreign import capi safe "BearLibTerminal.h terminal_color" c_terminal_color_uint :: CUInt -> IO ()
foreign import capi safe "BearLibTerminalExtras.h terminal_color_from_name" c_terminal_color_from_name :: CString -> IO ()

foreign import capi safe "BearLibTerminal.h terminal_bkcolor" c_terminal_bkcolor_uint :: CUInt -> IO ()
foreign import capi safe "BearLibTerminalExtras.h terminal_bkcolor_from_name" c_terminal_bkcolor_from_name :: CString -> IO ()

foreign import capi safe "BearLibTerminal.h terminal_composition" c_terminal_composition :: CInt -> IO ()

foreign import capi safe "BearLibTerminal.h terminal_layer" c_terminal_layer :: CInt -> IO ()

foreign import capi safe "BearLibTerminal.h terminal_clear" c_terminal_clear :: IO ()

foreign import capi safe "BearLibTerminal.h terminal_clear_area" c_terminal_clear_area :: CInt -> CInt -> CInt -> CInt -> IO ()

foreign import capi safe "BearLibTerminal.h terminal_crop" c_terminal_crop :: CInt -> CInt -> CInt -> CInt -> IO ()

foreign import capi safe "BearLibTerminal.h terminal_refresh" c_terminal_refresh :: IO ()

foreign import capi safe "BearLibTerminal.h terminal_put" c_terminal_put :: CInt -> CInt -> CInt -> IO ()

foreign import capi unsafe "BearLibTerminal.h terminal_pick" c_terminal_pick :: CInt -> CInt -> CInt -> IO CInt

foreign import capi unsafe "BearLibTerminal.h terminal_pick_color" c_terminal_pick_color :: CInt -> CInt -> CInt -> IO CUInt

foreign import capi unsafe "BearLibTerminal.h terminal_pick_bkcolor" c_terminal_pick_bkcolor :: CInt -> CInt -> IO CUInt

foreign import capi safe "BearLibTerminal.h terminal_put_ext" c_terminal_put_ext :: CInt -> CInt -> CInt -> CInt -> CInt -> Ptr CUInt -> IO ()

foreign import capi safe "BearLibTerminalExtras.h terminal_print_ptr" c_terminal_print_ptr :: CInt -> CInt -> CString -> Ptr Dimensions -> IO ()

foreign import capi safe "BearLibTerminalExtras.h terminal_print_ext_ptr" c_terminal_print_ext_ptr :: CInt -> CInt -> CInt -> CInt -> CInt -> CString -> Ptr Dimensions -> IO ()

-- I don't know if wchar is actually useful here.
-- I don't care enough to try and wrap va_list around the printf variants.
-- so that's printf, printf_ext, wprint, wprintf, wprint_ext, wprintf_ext, measuref, wmeasure, measuref_ext, wmeasuref_ext
-- check is unnecessary
-- foreign import capi unsafe "BearLibTerminal.h terminal_check" c_terminal_check :: CInt -> IO CInt
-- also read_wstr
-- not bothering with: color_from_name, color_from_argb

foreign import capi unsafe "BearLibTerminalExtras.h terminal_measure_ptr" c_terminal_measure_ptr :: CString -> Ptr Dimensions -> IO ()

foreign import capi unsafe "BearLibTerminalExtras.h terminal_measure_ext_ptr" c_terminal_measure_ext_ptr :: CInt -> CInt -> CString -> Ptr Dimensions -> IO ()

foreign import capi safe "BearLibTerminal.h terminal_state" c_terminal_state :: CInt -> IO CInt

foreign import capi safe "BearLibTerminal.h terminal_has_input" c_terminal_has_input :: IO CInt

foreign import capi safe "BearLibTerminal.h terminal_read" c_terminal_read :: IO CInt

foreign import capi safe "BearLibTerminal.h terminal_peek" c_terminal_peek :: IO CInt

foreign import capi safe "BearLibTerminal.h terminal_read_str" c_read_str :: CInt -> CInt -> Ptr CChar -> CInt -> IO CUInt

foreign import capi safe "BearLibTerminal.h terminal_delay" c_terminal_delay :: CInt -> IO ()