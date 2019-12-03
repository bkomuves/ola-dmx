
-- | Controlling the Involight LedStrob200 led strobe effect

{-# LANGUAGE BangPatterns, RecordWildCards #-}
module DMX.Lights.LedStrob200 where

--------------------------------------------------------------------------------

{- approximate frequencies

0.10 -> ~ 40 hz
0.20 -> ~ 55 hz
0.25 -> ~ 56 hz
0.3  -> ~ 65 hz
0.4  -> ~ 96 hz
0.5  ->   120 hz (??)
0.6  -> ~ 145 hz
0.75 -> ~ 240 hz

-}

--------------------------------------------------------------------------------

import DMX.Common

--------------------------------------------------------------------------------

instance DmxDeviceState LedStrobState where
  dmxDeviceName    _ = "Involight LedStrob 200"
  defaultDmxState  _ = StrobeOff
  dmxStateToValues   = ledStrobStateToValues

data LedStrobState
  = StrobeOff
  | SoundActive
  | DimmerMode 
      { dimmer :: !Double          -- ^ simple dimmer
      }
  | EffectMode 
      { effectIndex :: !Int     -- ^ which effect (1..8) 
      , effectParam :: !Double  -- ^ the effect parameter (0..1, but low resolution)
      , effectSpeed :: !Double  -- ^ 0..1
      }
  | StrobeMode
      { strobeDimmer :: !Double    -- ^ dimmer
      , strobeSpeed  :: !Double    -- ^ strobe speed
      }
  deriving Show

ledStrobStateToValues :: LedStrobState -> [Int]
ledStrobStateToValues state = case state of
  StrobeOff      -> [0,0,0,0]
  SoundActive    -> [0,0,x,0] where 
    x = 255
  DimmerMode dim -> [d,9,0,0] where
    d = doubleToIV (1, 255) dim
  StrobeMode{..} -> [a,b,0,0] where
    a = doubleToIV (1 ,255) strobeDimmer
    b = doubleToIV (10,255) strobeSpeed
  EffectMode{..} -> [0,0,c,d] where
    d = doubleToIV (0,255) effectSpeed
    j = mod (effectIndex - 1) 8 
    c = 60 + j*20 + doubleToIV (0,19) effectParam
    
--------------------------------------------------------------------------------
-- * internals

{-
doubleToIV :: (Int,Int) -> Double -> Int
doubleToIV (a,b) y0 = a + round (ba * y) where
  ba = fromIntegral (b-a) :: Double
  y  = min 1 (max 0 y0)
-}

--------------------------------------------------------------------------------
