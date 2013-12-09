module IslandLSystem (
  IslandModule (..),
  islandLSystem,
  islandInterpretation,
  ) where

import LSystem
import Turtle


data IslandModule = IslandLineWidth Double | IslandMove Double |
  IslandF Double | IslandL | IslandR

islandLSystem :: LSystem IslandModule
islandLSystem = LSystem (map LPrim [IslandLineWidth 5, IslandL, IslandL,
  IslandMove 100, IslandR, IslandMove 100, IslandR] ++
  (take 8 $ cycle [LPrim (IslandF 200.0), LPrim IslandR])) rules
  where
    rules (IslandF len) = [f, r, f, l, f, l, f, f, r, f, r, f, l, f]
      where
        f = LPrim $ IslandF (len/4)
        l = LPrim IslandL
        r = LPrim IslandR
    rules m = [LPrim m]

islandInterpretation :: IslandModule -> [LPrim TurtleModule]
islandInterpretation m = case m of
  IslandLineWidth wid -> [LPrim $ TSetLineWidth wid]
  IslandF len         -> [LPrim $ TDraw len]
  IslandMove len      -> [LPrim $ TMove len]
  IslandL             -> [LPrim (TTurnLeft 90)]
  IslandR             -> [LPrim (TTurnRight 90)]
