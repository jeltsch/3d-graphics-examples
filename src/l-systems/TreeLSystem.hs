module TreeLSystem (
  TreeModule (..),
  treeLSystem,
  treeInterpretation,
  ) where

import LSystem
import Turtle


data TreeModule =
  TreeA | TreeF Double | TreeEx Double | TreeLineWidth Double |
  -- Turn-, Roll und Pitch-Befehle
  TreeRR Double | TreePU Double | TreePD Double

treeLSystem :: LSystem TreeModule
treeLSystem = LSystem
  (map LPrim [TreePU 90, TreeEx 1, TreeF 200, TreeRR 45, TreeA]) rules
  where
    d1 = 112.50  -- divergence angle 1
    d2 = 157.50  -- divergence angle 2
    a  = 32.50   -- branching angle
    lr = 1.14    -- elongation rate
    vr = 1.732   -- width increase rate
    rules TreeA = [ LPrim $ TreeEx vr, LPrim $ TreeF 50,
      LStack [LPrim $ TreePD a, LPrim $ TreeF 50, LPrim TreeA],
        LPrim $ TreeRR d1,
      LStack [LPrim $ TreePD a, LPrim $ TreeF 50, LPrim TreeA],
        LPrim $ TreeRR d2,
      LStack [LPrim $ TreePD a, LPrim $ TreeF 50, LPrim TreeA] ]
    rules (TreeF len) = [LPrim $ TreeF (len*lr)]
    rules (TreeEx wid) = [LPrim $ TreeEx (wid*vr)]
    rules m = [LPrim m]

treeInterpretation :: TreeModule -> [LPrim TurtleModule]
treeInterpretation m = case m of
  TreeF len  -> [LPrim $ TDraw len]
  TreePU a   -> [LPrim $ TPitchUp a]
  TreePD a   -> [LPrim $ TPitchDown a]
  TreeRR a   -> [LPrim $ TRollRight a]
  TreeEx wid -> [LPrim $ TSetLineWidth wid]
  _ -> []
