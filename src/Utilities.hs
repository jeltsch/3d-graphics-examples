{-# LANGUAGE FlexibleInstances #-}
module Utilities (
  cBlack,
  cLightGray,
  cBlue,
  cGreen,
  cRed,
  cCyan,
  cMagenta,
  cYellow,
  cWhite,

  crMat,
  cCyanMaterial,

  projection,

  Num (..),
  realToReal,
  doubleToGLfloat,
  glfloatToDouble,

  interleave,
  inGroupsOf,
  lastAndInit,
  tailAndHead,

  showIO
  ) where


import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT


-- Einige Standard-Farben
cBlack   = Color4 0 0 0 (1::GLfloat)
cLightGray = Color4 0.7 0.7 0.7 (1::GLfloat)
cBlue    = Color4 0 0 1 (1::GLfloat)
cGreen   = Color4 0 1 0 (1::GLfloat)
cRed     = Color4 1 0 0 (1::GLfloat)
cCyan    = Color4 0 1 1 (1::GLfloat)
cMagenta = Color4 1 0 1 (1::GLfloat)
cYellow  = Color4 1 1 0 (1::GLfloat)
cWhite   = Color4 1 1 1 (1::GLfloat)


-- Lichtundurchlässiges Material erzeugen
crMat (rd,gd,bd) (rs,gs,bs) exp = do
  materialDiffuse Front $= Color4 rd gd bd 1.0
  materialAmbient Front $= Color4 rd gd bd 1.0
  materialSpecular Front $= Color4 rs gs bs 1.0
  materialShininess Front $= exp

  materialDiffuse Back $= Color4 rd gd bd 1.0
  materialSpecular Back $= Color4 rs gs bs 1.0
  materialShininess Back $= exp

cCyanMaterial :: IO ()
cCyanMaterial = crMat (0, 0.3, 0.3) (1, 1, 1.0) 5


-- Orthogonale Projektion
projection xl xu yl yu zl zu = do
  matrixMode $= Projection
  loadIdentity
  ortho xl xu yl yu zl zu
  matrixMode $= Modelview 0



-- Vektoraddition, -subtraktion und Kreuzprodukt
{-FIXME:
    It is not right to treat vectors as numbers. Introduce separate operators
    and then remove the LANGUAGE pragma above.
-}
instance (Num a, Num a, Num a) => Num (a, a, a) where
  (x1, y1, z1) + (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
  (x1, y1, z1) - (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
  (x1, y1, z1) * (x2, y2, z2) = (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2)
  fromInteger x               = (fromInteger x, fromInteger x, fromInteger x)
  abs (x, y, z)               = (abs x, abs y, abs z)
  signum (x, y, z)            = (signum x, signum y, signum z)


-- Konvertierung zwischen verschiedenen Real-Typen
realToReal :: (Real a, Fractional b) => a -> b
realToReal = fromRational . toRational

doubleToGLfloat :: Double -> GLfloat
doubleToGLfloat = realToReal

glfloatToDouble :: GLfloat -> Double
glfloatToDouble = realToReal


-- Mischt zwei Listen elementweise abwechselnd
interleave :: [a] -> [a] -> [a]
interleave [] bs = bs
interleave as [] = as
interleave (a:as) (b:bs) = a : b : interleave as bs


-- Teilt Liste in Unterlisten gegebener Länge auf
inGroupsOf :: [a] -> Int -> [[a]]
inGroupsOf [] _ = []
inGroupsOf cs n = lcs : inGroupsOf rcs n
  where (lcs, rcs) = splitAt n cs


-- Verschiebt das letzte Element einer Liste an den Anfang
lastAndInit :: [a] -> [a]
lastAndInit [] = []
lastAndInit cs = last cs : init cs


-- Verschiebt das erste Element einer Liste ans Ende
tailAndHead :: [a] -> [a]
tailAndHead [] = []
tailAndHead (c:cs) = cs ++ [c]


-- Führt IO-Aktion aus und gibt den Ergebniswert aus
showIO :: (Show a) => IO (a) -> IO ()
showIO ioVal = ioVal >>= putStr . show
