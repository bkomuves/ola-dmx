
-- | Sending DMX values via OLA (Open Lighting Architecture).
--
-- This uses the C++ API.
--
-- In my experience, this is more stable than the HTTP API.

{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
module DMX.OLA.Native 
  ( DMX
  , initDmxLogging
  , newDmxBuffer
  , freeDmxBuffer
  , getDmxBufferSize
  , dmxSetChannel
  , dmxSetBuffer
  , dmxSetBufferAt
  , dmxSendBuffer
  , dmxSendValues
  )
  where

--------------------------------------------------------------------------------

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal

--------------------------------------------------------------------------------

newtype DMX = DMX (Ptr DMX')

-- | Set up warning\/error logging to stderr
initDmxLogging :: IO ()
initDmxLogging = c_ola_init_logging

-- | Given a DMX universe (numbering starts from 1), we make a new \"dmx512 buffer\"
newDmxBuffer :: Int -> IO (Maybe DMX)
newDmxBuffer universe = do
  ptr <- c_ola_new_buffer (i2c universe)
  return $ if ptr /= nullPtr
    then Just (DMX ptr)
    else Nothing
    
freeDmxBuffer :: DMX -> IO ()
freeDmxBuffer (DMX ptr) = c_ola_free_buffer ptr

getDmxBufferSize :: DMX -> IO Int
getDmxBufferSize (DMX ptr) = c2i <$> c_ola_buffer_size ptr

-- | Sets the value of a single channel. Note: we don\'t actually send out the value!
dmxSetChannel 
  :: DMX 
  -> Int     -- ^ channel (0..511)
  -> Int     -- ^ value (0..255)
  -> IO ()
dmxSetChannel (DMX ptr) chn val = c_ola_set_channel ptr (i2c chn) (i2c val)

-- | Sets the first few element of a buffer
dmxSetBuffer :: DMX -> [Int] -> IO ()
dmxSetBuffer (DMX ptr) values = do
  withArrayLen (map i2c values) $ \n p -> c_ola_set_buffer ptr (i2c n) p

-- | Sets some elements of a buffer
dmxSetBufferAt :: DMX -> Int -> [Int] -> IO ()
dmxSetBufferAt (DMX ptr) startIdx values = do
  withArrayLen (map i2c values) $ \n p -> c_ola_set_buffer_at ptr (i2c startIdx) (i2c n) p

-- | Sends out all the values of the buffer
dmxSendBuffer :: DMX -> IO Bool
dmxSendBuffer (DMX ptr) = do
  status <- c_ola_send_buffer ptr
  return (status /= 0)
  
dmxSendValues :: DMX -> [Int] -> IO Bool
dmxSendValues (DMX ptr) values = do
  withArrayLen (map i2c values) $ \n p -> do
    status <- c_ola_send_values ptr (i2c n) p 
    return (status /= 0)

--------------------------------------------------------------------------------

c2i :: CInt -> Int
c2i = fromIntegral

i2c :: Int -> CInt
i2c = fromIntegral    

--------------------------------------------------------------------------------

{-

void ola_init_logging();

struct DMX *ola_new_buffer(int universe);
void ola_free_buffer( DMX *p );
int ola_buffer_size( DMX *p );

void ola_set_channel( DMX *p , int channel , int value );
void ola_set_buffer_at( DMX *p, int start , int len , int *values);
void ola_set_buffer   ( DMX *p ,            int len , int *values);
int ola_send_buffer( DMX *p );
int ola_send_values( DMX *p , int len , int *values);

-}

data DMX' = DMX'

foreign import ccall "ola_init_logging" c_ola_init_logging :: IO ()
foreign import ccall "ola_new_buffer"   c_ola_new_buffer   :: CInt -> IO (Ptr DMX')
foreign import ccall "ola_free_buffer"  c_ola_free_buffer  :: Ptr DMX' -> IO ()
foreign import ccall "ola_buffer_size"  c_ola_buffer_size  :: Ptr DMX' -> IO CInt

foreign import ccall "ola_set_channel"   c_ola_set_channel   :: Ptr DMX' -> CInt -> CInt -> IO ()
foreign import ccall "ola_set_buffer"    c_ola_set_buffer    :: Ptr DMX' -> CInt -> Ptr CInt -> IO ()
foreign import ccall "ola_set_buffer_at" c_ola_set_buffer_at :: Ptr DMX' -> CInt -> CInt -> Ptr CInt -> IO ()
foreign import ccall "ola_send_buffer"   c_ola_send_buffer   :: Ptr DMX' -> IO CInt
foreign import ccall "ola_send_values"   c_ola_send_values   :: Ptr DMX' -> CInt -> Ptr CInt -> IO CInt

--------------------------------------------------------------------------------

