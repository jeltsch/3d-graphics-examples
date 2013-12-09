module Turtle (
  TurtleModule (..)
  ) where


--   Turtle-Modul
--       ____
--     _//__\\=o
--      ^    ^

data TurtleModule =
  TDraw Double |  -- L�nge len vorw�rts bewegen und eine Linie zeichnen
  TMove Double |  -- L�nge len vorw�rts bewegen ohne eine Linie zu zeichnen

  TTurnLeft Double |    -- alpha Grad links um die Up-Achse drehen
  TTurnRight Double |   -- alpha Grad rechts um die Up-Achse drehen
  TTurnAround |         -- 180 Grad um die Up-Achse drehen

  TPitchDown Double |   -- alpha Grad auf der Left-Achse nach unten neigen
  TPitchUp Double |     -- alpha Grad auf der Left-Achse nach oben neigen

  TRollLeft Double |    -- alpha Grad auf der Head-Achse nach links rollen
  TRollRight Double |   -- alpha Grad auf der Head-Achse nach rechts rollen

  TSetLineWidth Double  -- Linienbreite setzen

  deriving (Eq, Show)
