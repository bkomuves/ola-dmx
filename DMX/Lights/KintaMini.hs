
-- | Controlling the Chauvet DJ Kinta Mini effect light

{-# LANGUAGE BangPatterns, RecordWildCards #-}
module DMX.Lights.KintaMini where

--------------------------------------------------------------------------------

import Data.Char

import DMX.Common

--------------------------------------------------------------------------------

{-
approximate strobe frequencies
  0      -> 0.5 hz
  0.5    ->   1 hz
  0.75   ->   2 hz
  0.875  ->   4 hz
  0.9375 ->   8 hz
-}

--------------------------------------------------------------------------------

kintaMini
  :: LightCombo                  -- ^ lights on\/off
  -> Motor                       -- ^ motor speed or position
  -> Maybe Double                -- ^ strobe frequency
  -> KintaMiniState       
kintaMini lights = kintaMini' (Right lights)

kintaMiniBinary
  :: LightsBool                  -- ^ lights on\/off
  -> Motor                       -- ^ motor speed or position
  -> Maybe Double                -- ^ strobe frequency
  -> KintaMiniState       
kintaMiniBinary lights = kintaMini' (Right $ lightsToCombo lights)

kintaMini' 
  :: Either AutoLight LightCombo    -- ^ lights on\/off
  -> Motor                          -- ^ motor speed or position
  -> Maybe Double                   -- ^ strobe frequency
  -> KintaMiniState       
kintaMini' = KintaMiniState

--------------------------------------------------------------------------------
-- * state

instance DmxDeviceState KintaMiniState where
  dmxDeviceName    _ = "Chauvet DJ Kinta Mini"
  defaultDmxState  _ = kintaMiniDefault
  dmxStateToValues   = kintaMiniStateToValues

kintaMiniDefault = KintaMiniState
  { kintaLights = Right Off
  , kintaMotor  = MotorPosition 0
  , kintaStrobo = Nothing
  }
  
data KintaMiniState = KintaMiniState
  { kintaLights :: !(Either AutoLight LightCombo)
  , kintaMotor  :: !(Motor)
  , kintaStrobo :: !(Maybe Double)     -- ^ strobe frequency
  }
  deriving Show

kintaMiniStateToValues :: KintaMiniState -> [Int]  
kintaMiniStateToValues (KintaMiniState ei motor mbStrobo) = [a,b,c] where
  a = case ei of
    Left  auto  -> autoLightToInt  auto
    Right combo -> lightComboToInt combo
  b = case mbStrobo of
    Nothing -> 0
    Just y  -> 6 + round (249 * min 1 (max 0 y))
  c = motorToInt motor

--------------------------------------------------------------------------------
-- * types
  
-- | Turn the individual lights on\/off 
--
-- Unfortunately the Red+Blue+White combination is /missing/ from the
-- protocol... 
data LightsBool = LightsBool
  { red   :: !Bool
  , green :: !Bool
  , blue  :: !Bool
  , white :: !Bool
  }
  deriving Show

-- | Lights automatically changing
data AutoLight 
  = Auto4Color     -- ^ 4-color chase
  | Auto7Color     -- ^ 7-color chase
  deriving Show

{-
data Motor 
  = MotorSpeed    !Double    -- ^ the motor is rotating (0..1)
  | MotorPosition !Double    -- ^ the motor is fixed at the given position (0..1)
  deriving Show
-}

--------------------------------------------------------------------------------

data LightCombo 
  = Off 
  | R   | G   | B   | W 
  | RG  | RB  | RW  | GB  | GW  | BW
  | RGB | RGW | GBW
  | RGBW
  deriving Show

parseLightCombo :: String -> LightCombo
parseLightCombo s = case mbParseLightCombo s of 
  Just y  -> y
  Nothing -> error "parseLightCombo: cannot parse"
  
mbParseLightCombo :: String -> Maybe LightCombo
mbParseLightCombo s = case map toLower s of 
  ""     -> Just Off
  "r"    -> Just R
  "g"    -> Just G
  "b"    -> Just B
  "w"    -> Just W
  "rg"   -> Just RG
  "rb"   -> Just RB
  "rw"   -> Just RW
  "gb"   -> Just GB
  "gw"   -> Just GW
  "bw"   -> Just BW
  "rgb"  -> Just RGB
  "rgw"  -> Just RGW
  "gbw"  -> Just GBW
  "rgbw" -> Just RGBW  
  "off"  -> Just Off
  "none" -> Just Off
  _      -> Nothing
  
--------------------------------------------------------------------------------
-- * internals

{-
motorToInt :: Motor -> Int
motorToInt m = case m of
  MotorPosition x -> doubleToIV (0  ,127) x
  MotorSpeed    x -> doubleToIV (128,255) x
-}

{-
motorToInt :: Motor -> Int
motorToInt m = case m of
  MotorPosition x ->        mod x 128
  MotorSpeed    x -> 128 + (mod x 128)
-}

autoLightToInt :: AutoLight -> Int
autoLightToInt auto = case auto of
  Auto4Color -> 220                  -- 216..230 
  Auto7Color -> 240                  -- 231..255

lightsToInt :: LightsBool -> Int
lightsToInt lights = div (a+b) 2 where (a,b) = lightsToIV lights

lightsToIV :: LightsBool -> (Int,Int)
lightsToIV = lightComboToIV . lightsToCombo 

lightsToCombo :: LightsBool -> LightCombo
lightsToCombo (LightsBool r g b w) = case (r,g,b,w) of
  (False, False, False, False) -> Off  -- (  0 ,   5 )
  (True , False, False, False) -> R    -- (  6 ,  20 )    -- R
  (False, True , False, False) -> G    -- ( 21 ,  35 )    -- G
  (False, False, True , False) -> B    -- ( 36 ,  50 )    -- B
  (False, False, False, True ) -> W    -- ( 51 ,  65 )    -- W
  (True , True , False, False) -> RG   -- ( 66 ,  80 )    -- RG
  (True , False, True , False) -> RB   -- ( 81 ,  95 )    -- RB
  (True , False, False, True ) -> RW   -- ( 96 , 110 )    -- RW
  (False, True , True , False) -> GB   -- (111 , 125 )    -- GB
  (False, True , False, True ) -> GW   -- (126 , 140 )    -- GW       
  (False, False, True , True ) -> BW   -- (141 , 155 )    -- BW        
  (True , True , True , False) -> RGB  -- (156 , 170 )    -- RGB
  (True , True , False, True ) -> RGW  -- (171 , 185 )    -- RGW
  (False, True , True , True ) -> GBW  -- (186 , 200 )    -- GBW
  (True , False, True , True ) -> RGBW -- (201 , 215 )    -- R(g)BW   -- Red/Blue/White IS MISSING FROM THE PROTOCOL, W.T.F
  (True , True , True , True ) -> RGBW -- (201 , 215 )    -- RGBW  

lightComboToInt :: LightCombo -> Int
lightComboToInt combo = div (a+b) 2 where (a,b) = lightComboToIV combo

lightComboToIV :: LightCombo -> (Int,Int)
lightComboToIV combo = case combo of
  Off  -> (  0 ,   5 )
  R    -> (  6 ,  20 )    -- R
  G    -> ( 21 ,  35 )    -- G
  B    -> ( 36 ,  50 )    -- B
  W    -> ( 51 ,  65 )    -- W
  RG   -> ( 66 ,  80 )    -- RG
  RB   -> ( 81 ,  95 )    -- RB
  RW   -> ( 96 , 110 )    -- RW
  GB   -> (111 , 125 )    -- GB
  GW   -> (126 , 140 )    -- GW       
  BW   -> (141 , 155 )    -- BW        
  RGB  -> (156 , 170 )    -- RGB
  RGW  -> (171 , 185 )    -- RGW
  GBW  -> (186 , 200 )    -- GBW
  RGBW -> (201 , 215 )    -- RGBW  

--------------------------------------------------------------------------------
