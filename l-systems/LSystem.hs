module LSystem (
  LSystem (..),
  LPrim (..),

  derivations,
  interprete,
  interpretations
  ) where

import Turtle


-- LSystem module = LSystem axiom ableitungsregeln
data LSystem m = LSystem [LPrim m] (m -> [LPrim m])

data LPrim m = LPrim m | LStack [LPrim m]



-------------------------------------------------------------------------------


-- Einzelnes, primitives Modul ableiten
derive :: (m -> [LPrim m]) -> LPrim m -> [LPrim m]
derive rules x = case x of
  LPrim p  -> rules p
  LStack s -> [LStack $ concatMap (derive rules) s]


-- Erzeugt Liste aller Ableitungen eines L-Systems
derivations :: LSystem m -> [ [LPrim m] ]
derivations (LSystem startword rules) = derivations'
  where
    derivations' = startword : map (concatMap $ derive rules) derivations'


-- Übersetzt ein Wort von Modulen des Typs m in ein Wort von Modulen des Typs
-- TurtleModule
interprete :: (m -> [LPrim TurtleModule]) -> [m] -> [LPrim TurtleModule]
interprete = concatMap


-- Wendet interprete auf eine Liste von Modulworten an
interpretations :: (m -> [LPrim TurtleModule]) -> [[LPrim m]] -> [[LPrim TurtleModule]]
interpretations rules = map (interprete intPrim)
  where
    intPrim x = case x of
      LPrim p  -> rules p
      LStack s -> [LStack $ (interprete intPrim) s]
