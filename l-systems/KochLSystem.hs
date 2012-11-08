module KochLSystem (
  KochModule (..),
  kochLSystem,
  kochInterpretation
  ) where

import LSystem
import Turtle


data KochModule = KochF Double | KochLeft | KochRight | KochLineWidth Double


kochLSystem :: LSystem KochModule
kochLSystem = LSystem ([LPrim $ KochLineWidth 5] ++ (take 9 $ cycle
    [LPrim $ KochF 200.0, LPrim KochRight, LPrim KochRight])) rules
  where
    rules (KochF len) = map (\p -> LPrim p)
      [KochF (len/3), KochLeft, KochF (len/3), KochRight,
        KochRight, KochF (len/3), KochLeft, KochF (len/3)]
    rules m = [LPrim m]


kochInterpretation :: KochModule -> [LPrim TurtleModule]
kochInterpretation m = case m of
  KochLineWidth wid -> [LPrim $ TSetLineWidth wid]
  KochF len         -> [LPrim $ TDraw len]
  KochLeft          -> [LPrim $ TTurnLeft 60]
  KochRight         -> [LPrim $ TTurnRight 60]
