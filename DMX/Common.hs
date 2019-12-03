
-- | Shared miscellaneous code

{-# LANGUAGE BangPatterns, RecordWildCards #-}
module DMX.Common where

--------------------------------------------------------------------------------

import Data.Proxy

--------------------------------------------------------------------------------

doubleToIV :: (Int,Int) -> Double -> Int
doubleToIV (a,b) y0 = a + round (ba * y) where
  ba = fromIntegral (b-a) :: Double
  y  = min 1 (max 0 y0)

--------------------------------------------------------------------------------

class DmxDeviceState state where
  dmxDeviceName    :: Proxy state -> String
  defaultDmxState  :: Proxy state -> state
  dmxStateToValues :: state -> [Int]

--------------------------------------------------------------------------------

data Motor 
  = MotorSpeed    !Double    -- ^ the motor is rotating (0..1)
  | MotorPosition !Double    -- ^ the motor is fixed at the given position (0..1)
  deriving Show

motorToInt :: Motor -> Int
motorToInt m = case m of
  MotorPosition x -> doubleToIV (0  ,127) x
  MotorSpeed    x -> doubleToIV (128,255) x

--------------------------------------------------------------------------------
