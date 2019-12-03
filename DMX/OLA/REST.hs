
-- | OLA via the OLA daemon (called @olad@) HTTP REST API

module DMX.OLA.REST where

--------------------------------------------------------------------------------

import Control.Monad
import Control.Concurrent.MVar

import Data.List

import System.IO.Unsafe as Unsafe

import Network.HTTP

--------------------------------------------------------------------------------

{-
curl examples:

curl -X GET                        http://127.0.0.1:9090/get_dmx?u=1
curl -X POST -d "u=1&d=10,200,150" http://127.0.0.1:9090/set_dmx
-}

--------------------------------------------------------------------------------

theOLAServerURL :: MVar String
theOLAServerURL = Unsafe.unsafePerformIO $ newMVar "http://127.0.0.1:9090"

setOLAServer 
  :: String    -- ^ olad URL, for example @http://127.0.0.1:9090@ (which is the default)
  -> IO ()
setOLAServer url = do
  _ <- tryTakeMVar theOLAServerURL
  putMVar theOLAServerURL url
  
--------------------------------------------------------------------------------

-- | Set DMX values
setDMX 
  :: Int     -- ^ DMX universe
  -> [Int]   -- ^ values, 0..255 (starting from channel 0)
  -> IO ()
setDMX universe values = do
  server <- readMVar theOLAServerURL
  let f x = show (mod x 256)
      queryString = "u=" ++ show universe ++ "&d=" ++ intercalate "," (map f values)
      url = server ++ "/set_dmx"
      contentType = "application/x-www-form-urlencoded"
  response <- simpleHTTP $ postRequestWithBody url contentType queryString
  -- print queryString
  -- print response
  return ()
  
-- | Get DMX values
getDMX 
  :: Int     -- ^ DMX universe  
  -> IO [Int]
getDMX universe = do
  server <- readMVar theOLAServerURL
  let url = server ++ "/get_dmx?u=" ++ show universe
  response <- simpleHTTP $ getRequest url
  return $ error "not implemented"
  
--------------------------------------------------------------------------------
  