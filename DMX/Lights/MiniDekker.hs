
-- | Controlling the American DJ Mini Dekker effect light

{-# LANGUAGE BangPatterns, RecordWildCards #-}
module DMX.Lights.MiniDekker where

--------------------------------------------------------------------------------

import DMX.Common

--------------------------------------------------------------------------------

type Strobe = Double

{-
approximate strobe frequencies ?????????
  0      -> 0.5 hz 
  0.5    ->   1 hz
  0.75   ->   2 hz
  0.875  ->   4 hz
  0.9375 ->   8 hz
-}

--------------------------------------------------------------------------------
-- * state

instance DmxDeviceState MiniDekkerState where
  dmxDeviceName    _ = "American DJ Mini Dekker"
  defaultDmxState  _ = miniDekkerDefault
  dmxStateToValues   = miniDekkerStateToValues
  
miniDekkerDefault = MiniDekkerState
  { dekkerLights = Lights 0 0 0 0
  , dekkerDimmer = 1
  , dekkerMotor  = MotorPosition 0
  , dekkerStrobo = Nothing
  , dekkerColSel = Nothing
  }
  
data MiniDekkerState = MiniDekkerState
  { dekkerLights :: !Lights
  , dekkerDimmer :: !Double
  , dekkerMotor  :: !(Motor)
  , dekkerStrobo :: !(Maybe Strobe)       -- frequency
  , dekkerColSel :: !(Maybe ColorSelect)
  }
  deriving Show

-- | Color selection
data ColorSelect
  = ColorMixing
  | ColorFading !Double
  deriving Show
    
miniDekkerStateToValues :: MiniDekkerState -> [Int]  
miniDekkerStateToValues MiniDekkerState{..} = [r,g,b,w,s,d,m,c,f] where
  r = doubleToIV (0,255) $ red   dekkerLights
  g = doubleToIV (0,255) $ green dekkerLights
  b = doubleToIV (0,255) $ blue  dekkerLights
  w = doubleToIV (0,255) $ white dekkerLights
  s = mbStrobeToInt      $ dekkerStrobo
  d = doubleToIV (0,255) $ dekkerDimmer
  m = motorToInt         $ dekkerMotor
  c = mbColSelectToInt   $ dekkerColSel
  f = 0                                         -- dmx control

--------------------------------------------------------------------------------
-- * types
  
-- | Dimming the individual lights (0=off, 1=max) 
--
-- Unfortunately the Red+Blue+White combination is /missing/ from the
-- protocol... 
data Lights = Lights
  { red   :: !Double
  , green :: !Double
  , blue  :: !Double
  , white :: !Double
  }
  deriving Show
  
noLights, allLights :: Lights
noLights  = Lights 0 0 0 0
allLights = Lights 1 1 1 1

{-
data Motor 
  = MotorSpeed    !Double    -- ^ the motor is rotating (0..1)
  | MotorPosition !Double    -- ^ the motor is fixed at the given position (0..1)
  deriving Show
-}

--------------------------------------------------------------------------------
-- * internals

{-
doubleToIV :: (Int,Int) -> Double -> Int
doubleToIV (a,b) y0 = a + round (ba * y) where
  ba = fromIntegral (b-a) :: Double
  y  = min 1 (max 0 y0)
-}

mbStrobeToInt :: Maybe Strobe -> Int
mbStrobeToInt mb = case mb of
  Nothing -> 5                      --  0..9
  Just x  -> doubleToIV (10,255) x  -- 10..255
  
{-
motorToInt :: Motor -> Int
motorToInt m = case m of
  MotorPosition x -> doubleToIV (0  ,127) x
  MotorSpeed    x -> doubleToIV (128,255) x
-}

mbColSelectToInt :: Maybe ColorSelect -> Int
mbColSelectToInt mb = case mb of
  Nothing -> 0              -- 0..9
  Just cs -> case cs of
    ColorMixing   -> doubleToIV (10, 84) 0.5   -- 10..84
    ColorFading y -> doubleToIV (85,255) y     -- 85..255
    
--------------------------------------------------------------------------------
