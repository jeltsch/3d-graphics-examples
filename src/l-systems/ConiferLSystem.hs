module ConiferLSystem (
  ConiferModule (..),
  coniferLSystem,
  coniferInterpretation,
  ) where

import LSystem
import Turtle


data ConiferModule =
  ConiferX Double | ConiferY Double | ConiferF Double |
  ConiferLineWidth Double |
  ConiferTL Double | ConiferTR Double |  -- Turn-Befehle
  ConiferRL Double | ConiferRR Double |  -- Roll-Befehle
  ConiferPU Double | ConiferPD Double    -- Pitch-Befehle


coniferLSystem :: LSystem ConiferModule
coniferLSystem =
  LSystem (map LPrim [ConiferLineWidth 10, ConiferPU 90, ConiferX 20]) rules
  where
    rules (ConiferX a)
      | a > 6     = [ LPrim $ ConiferF 5,
        LStack [ LPrim $ ConiferPD 80, LPrim $ ConiferY (a/2) ],
        LPrim $ ConiferRR 137, LPrim $ ConiferX (a-1) ]
      | otherwise = [ LPrim $ ConiferF 6 ]
    rules (ConiferY a)
      | a >= 3    = [ LPrim $ ConiferF 3,
        LStack [ LPrim $ ConiferTL 50, LPrim $ ConiferF a ],
        LPrim $ ConiferRR 180, LPrim $ ConiferY (a-1) ]
      | otherwise = [ LPrim $ ConiferF 3 ]
    rules m = [LPrim m]


coniferInterpretation :: ConiferModule -> [LPrim TurtleModule]
coniferInterpretation m = case m of
  ConiferF len         -> [LPrim $ TDraw (10*len)]
  ConiferTL a          -> [LPrim $ TTurnLeft a]
  ConiferTR a          -> [LPrim $ TTurnRight a]
  ConiferPU a          -> [LPrim $ TPitchUp a]
  ConiferPD a          -> [LPrim $ TPitchDown a]
  ConiferRL a          -> [LPrim $ TRollLeft a]
  ConiferRR a          -> [LPrim $ TRollRight a]
  ConiferLineWidth wid -> [LPrim $ TSetLineWidth wid]
  _ -> []
