module Main (main) where

--------------------------------------------------------------------------------
--
-- Parkettierf�ige Brown'sche 2D-Fl�he (Diamond-Square-Algorithums)
--
--
-- Tastenbelegungen im Hauptfenster:
-- Links, Rechts              -> Ansicht drehen
-- Oben, Unten                -> Blickh�enwinkel �dern
-- Strg + Pfeiltaste          -> Ansicht verschieben
-- '+', '-'                   -> Ableitungsschritt �dern
-- 'I', 'O'                   -> Hinein- bzw. Hinauszoomen
-- 'S'                        -> Darstellungsmodus: gefllte Polygone
-- 'W'                        -> Darstellungsmodus: Drahtgitter
-- 'P'                        -> Darstellungsmodus: H�en als Punkte
-- 'F'                        -> zwischen flachem Wasser und
--                               Wassertiefendarstellung wechseln
-- 'N'                        -> Neues Zufallsterrain erzeugen
--
--------------------------------------------------------------------------------


import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT hiding (initState)

import MyUtilities

import Data.List
import Data.IORef
import System.Random
import Data.Char (toUpper)


type Pair t = (t, t)
type Tupel3 t = (t, t, t)

data TerrainDrawMode = TerrainPoints | TerrainWireframe | TerrainSolid
  deriving (Eq, Show)

data Distribution = UniformDistribution | NormalDistribution
  deriving (Eq, Show)

data State = State {
  iteration  :: Int,              -- Ableitungsschritt

  viewPhi    :: Double,           -- Drehwinkel des Terrains um die z-Achse
  viewTheta  :: Double,           -- Blickwinkel (zwischen Boden und Zenit)
  zoom       :: Double,           -- Vergr�erungsfaktor
  pan        :: (Double, Double), -- Verschiebung

  roughness  :: Double,           -- Rauhigkeit
  drawMode   :: TerrainDrawMode,  -- Zeichenmodus
  terrainNumber :: Int,           -- Initialwert fr Zufallsgenerator
  flattenWater  :: Bool,          -- True  -> Wasser zu Ebene abflachen
                                  -- False -> Wassertiefen darstellen
  renderingRequired :: Bool       -- Mu�Terrain neu gerendert werden?
}

initState = State {
  iteration = 1,

  viewPhi   = -30,
  viewTheta = 60,
  zoom      = 1,
  pan       = (0,0),

  roughness = global_roughness,
  drawMode  = TerrainSolid,
  terrainNumber = 0,
  flattenWater = False,
  renderingRequired = True
}


global_roughness    = 0.4 -- Rauhigkeit des Terrains, Standard: 0.5
                           -- 0,..,0.5 = rauher; 0.5,..,1.0 = glatter
global_distribution = UniformDistribution
global_windowTitle  = "Fraktale Gebirgslandschaft"
global_windowSizeX  = 640
global_windowSizeY  = 480

maximumBound    = 1.0
clearWaterLimit = -0.315
waterLimit      = -0.3
vegetationLimit = 0.1
rockLimit       = 0.4
snowLimit       = 1.0

cSnow       = Color4 1.0 1.0 1.0 (1.0::GLfloat)
cDarkRock   = Color4 0.5 0.5 0.5 (1.0::GLfloat)
cLightRock  = Color4 0.25 0.25 0.25 (1.0::GLfloat)
cDarkVeg    = Color4 0.1 0.25 0.1 (1.0::GLfloat)
cLightVeg   = Color4 0.1 0.7 0.2 (1.0::GLfloat)
cClearWater = Color4 0.1 0.7 0.9 (0.35::GLfloat)
cLightWater = Color4 0.1 0.3 0.7 (1.0::GLfloat)
cDarkWater  = Color4 0.0 0.1 0.3 (1.0::GLfloat)



-- Main-Funktion ---------------------------------------------------------------


main = do
  getArgsAndInitialize
  initialDisplayMode $= [RGBAMode, WithDepthBuffer, DoubleBuffered,
    WithAlphaComponent]

  createWindow global_windowTitle
  windowSize $= Size global_windowSizeX global_windowSizeY

  shadeModel $= Smooth
  depthFunc $= Just Less
  normalize $= Enabled

  state <- newIORef initState

  displayCallback $= display state
  keyboardMouseCallback $= Just (keyboard state)

  mainLoop



-- Hauptzeichenfunktion --------------------------------------------------------


display state = do
  curState <- get state

  clear [DepthBuffer, ColorBuffer]

  let z = realToReal $ zoom curState
  projection (-z) (z) (-z) (z) (-100) (100)
  loadIdentity

  let (xPan, yPan) = pan curState

  translate $ Vector3 (doubleToGLfloat xPan) (doubleToGLfloat yPan) 0

  rotate (doubleToGLfloat $ negate $ viewTheta curState) $ Vector3 1 0 0
  rotate (doubleToGLfloat $ viewPhi curState) $ Vector3 0 0 1

  let
    d = 0.6
    flat = \z -> if z < clearWaterLimit then clearWaterLimit else z
    initGen = mkStdGen (terrainNumber curState)
    (matrix, _) = (allTerrainSteps initGen) !! (iteration curState)
    matrix' = if (flattenWater curState)
      then map (map (flat)) matrix else matrix

  if (renderingRequired curState)
    then defineList (DisplayList 1) CompileAndExecute $ do
      blend $= Disabled
      drawTerrain (drawMode curState) (-d,-d) (d,d) $ enlargeTerrain matrix'

      if (drawMode curState == TerrainSolid)
        then if (not $ flattenWater curState)
          then do
            blend $= Enabled
            blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
            drawClearWaterPlane (-d,-d) (d,d)
          else return ()
        else return ()
    else do
      callList (DisplayList 1)

  windowTitle $= global_windowTitle ++
    " (Terrain " ++ (show $ terrainNumber curState) ++
    ", n=" ++ (show $ iteration curState) ++ ")"
  state $= curState { renderingRequired = False }
  swapBuffers



-- Keyboard-Ereignisverarbeitung -----------------------------------------------


keyboard state (SpecialKey specialKey) Down mod _ = do
  curState <- get state
  let
    phi = viewPhi curState
    theta = viewTheta curState
    ctrlPressed = ctrl mod == Down
    z = zoom curState
    (x,y) = pan curState

  if ctrlPressed
    then case specialKey of
      KeyLeft  -> state $= curState { pan = (x - 0.05 * z, y) }
      KeyRight -> state $= curState { pan = (x + 0.05 * z, y) }
      KeyDown  -> state $= curState { pan = (x, y - 0.05 * z) }
      KeyUp    -> state $= curState { pan = (x, y + 0.05 * z) }
      _ -> return ()
    else case specialKey of
      KeyLeft  ->
        state $= curState { viewPhi = if phi<0 then phi+355 else phi-5 }
      KeyRight ->
        state $= curState { viewPhi = if phi>360 then phi-355 else phi+5 }
      KeyDown  ->
        state $= curState { viewTheta = if theta<=0 then 0 else theta-5 }
      KeyUp ->
        state $= curState { viewTheta = if theta>=90 then 90 else theta+5 }
      _ -> return ()

  postRedisplay Nothing

keyboard state (Char charKey) Down _ _ = do
  curState <- get state
  let
    i = iteration curState
    z = zoom curState

  state $= curState { renderingRequired = True }
  curState <- get state

  case toUpper charKey of
    'I' -> do
      state $= curState { zoom = z / 1.05, renderingRequired = False }
      postRedisplay Nothing
    'O' -> do
      state $= curState { zoom = z * 1.05, renderingRequired = False }
      postRedisplay Nothing
    '+' -> do
      state $= curState { iteration = i + 1 }
      postRedisplay Nothing
    '-' -> do
      state $= curState { iteration = if i <= 0 then 0 else i - 1 }
      postRedisplay Nothing
    'F' -> do
      let flatten = flattenWater curState
      state $= curState { flattenWater = not flatten }
      postRedisplay Nothing
    'N' -> do
      newTerrainNumber <- (randomIO :: IO Int)
      state $= curState { terrainNumber = newTerrainNumber }
      postRedisplay Nothing
    'P' -> do
      state $= curState { drawMode = TerrainPoints }
      postRedisplay Nothing
    'S' -> do
      state $= curState { drawMode = TerrainSolid }
      postRedisplay Nothing
    'W' -> do
      state $= curState { drawMode = TerrainWireframe }
      postRedisplay Nothing
    _ -> state $= curState { renderingRequired = False }

keyboard _ _ _ _ _ = return ()



-- Terrain-Zeichenfunktionen ---------------------------------------------------


-- pointsToQuadStrip mode (xl,yl) (xr,yr) lzs rzs
--
-- Zeichnet ein QuadStrip mit den parallel nebeneinander gelegten H�enwerten
-- aus lzs und rzs zwischen dem Anfangspunkt (xl,yl) und dem Endpunkt (xr,yr)
-- mit dem gegebenen Zeichenmodus mode.
--
pointsToQuadStrip :: TerrainDrawMode -> (Double,Double) -> (Double,Double)
  -> [Double] -> [Double] -> IO ()
pointsToQuadStrip mode (xl,yl) (xr,yr) lzs rzs = do
  let
    n = length lzs - 1
    dx = (xr - xl) / fromIntegral n
    dy = (yr - yl) / fromIntegral n

    xs = concat [ [x,x] | x <- [xl + dx*fromIntegral i | i<-[0..n]] ]
    ys = cycle [yl,yr]
    zs = interleave lzs rzs

  case mode of
    TerrainSolid -> renderPrimitive QuadStrip $ do
      sequence_ $ zipWith3 toColorVertex xs ys zs
    TerrainPoints -> renderPrimitive Points $ do
      sequence_ $ zipWith3 toColorVertex xs ys zs
    _ -> return ()


-- Gibt die dem H�enwert zugeordnete Farbe zurck
chooseColor z
  | z <= waterLimit = return $
    interpolateColor (-maximumBound, cDarkWater) (waterLimit, cLightWater) z
  | z <= vegetationLimit = return $
    interpolateColor (waterLimit, cLightVeg) (vegetationLimit, cDarkVeg) z
  | z <= rockLimit = return $
    interpolateColor (vegetationLimit, cDarkRock) (rockLimit, cLightRock) z
  | otherwise = return cSnow


-- interpolateColor (lower, colorL) (upper, colorU) height
--
-- Gibt eine linear interpolierte Farbe zwischen colorL und colorU zurck
--
interpolateColor (lower, colorL) (upper, colorU) height = Color4 rH gH bH aH
  where
    aH = max aL aU
    bH = bL + diff*(bU-bL)
    gH = gL + diff*(gU-gL)
    rH = rL + diff*(rU-rL)
    Color4 rU gU bU aU = colorU
    Color4 rL gL bL aL = colorL
    diff = (fromRational.toRational) $ (height-lower) / (upper-lower)


-- Farbe entsprechend z-Wert w�len und gef�bten Vertex setzen
toColorVertex x y z = do
  color <- chooseColor z
  currentColor $= color
  vertex $ Vertex3 (doubleToGLfloat x) (doubleToGLfloat y) (doubleToGLfloat z)


-- pointsToLines (x0,y0) (xn,yn) zs
--
-- Zeichnet eine gebrochene Linie mit den gleichm�ig verteilten H�enwerten
-- aus zs zwischen dem Anfangspunkt (x0,y0) und dem Endpunkt (xn,yn).
--
pointsToLines :: (Double,Double) -> (Double,Double) -> [Double] -> IO ()
pointsToLines (x0,y0) (xn,yn) zs = do
  let
    n = length zs - 1
    dx = (xn - x0) / fromIntegral n
    dy = (yn - y0) / fromIntegral n

    toLine i zl zr = do
      let
        yl = y0 + dy * fromIntegral i
        xl = x0 + dx * fromIntegral i
      renderPrimitive Lines $ do
        toColorVertex xl yl zl
        toColorVertex (xl+dx) (yl+dy) zr

  sequence_ $ zipWith3 toLine [0..n] zs $ tail zs


-- drawTerrain mode (xl,yl) (xr,yr) hss
--
-- Zeichnet das Terrain mit den H�enwerten hss zwischen den Punkten (xl,yl)
-- und (xr,yr) mit dem Zeichenmodus mode.
--
drawTerrain :: TerrainDrawMode -> Pair Double -> Pair Double -> [[Double]]
  -> IO ()
drawTerrain mode (xl,yl) (xr,yr) hss@(fhs:_) = do
  let
    rps = [ (xr, y + dy) | (_,y) <- lps ]
    lps = [ (xl, yl + dy*fromIntegral i) | i <- [0..n-1] ]
    drawStrips =
      sequence_ $ zipWith4 (pointsToQuadStrip mode) lps rps hss (tail hss)

  if mode == TerrainWireframe
    then do
      sequence_ $ zipWith3 pointsToLines xly xry hss
      sequence_ $ zipWith3 pointsToLines xyl xyr hssT
    else drawStrips

  where
    hssT = transpose hss

    xyr = [(x, yr) | (x,_) <- xyl]
    xyl = [(xl + dx * fromIntegral i, yl) | i<-[0..n]]

    xry = [(xr, y) | (_,y) <- xly]
    xly = [(xl, yl + dy * fromIntegral i) | i<-[0..n]]

    dy = (yr - yl) / fromIntegral n
    dx = (xr - xl) / fromIntegral n
    n = length fhs - 1


-- Zeichnet halbtransparente Wasserfl�he zwischen zwei Punkten.
drawClearWaterPlane (xl,yl) (xr,yr) = renderPrimitive Quads $ do
  currentColor $= cClearWater
  vertex $ Vertex3 (doubleToGLfloat xl) (doubleToGLfloat yl) (doubleToGLfloat clearWaterLimit)
  vertex $ Vertex3 (doubleToGLfloat xr) (doubleToGLfloat yl) (doubleToGLfloat clearWaterLimit)
  vertex $ Vertex3 (doubleToGLfloat xr) (doubleToGLfloat yr) (doubleToGLfloat clearWaterLimit)
  vertex $ Vertex3 (doubleToGLfloat xl) (doubleToGLfloat yr) (doubleToGLfloat clearWaterLimit)



-- Terrain-Berechnung ----------------------------------------------------------


-- logBase2 z liefert n mit 2^n = z
logBase2 :: Int -> Int
logBase2 z
  | z `mod` 2 == 0 = 1 + logBase2 (z `div` 2)
  | otherwise = 0


-- variance i h = Varianz mit Rauhigkeit h im i-ten Schrit der Terrainableitung
variance :: Double -> Double -> Double
variance i h = (1-2**(2*h-2)) / (2**(2*h*i))


-- Unendliche Liste von gleichverteilten Zufallswerten mit zugeh�igem
-- neuen Generator
allUniforms :: RandomGen gen => gen -> Double -> [(Double, gen)]
allUniforms gen var = (x, nextGen) : allUniforms nextGen var
  where
    (x, nextGen) = randomR (-var, var) gen


-- Unendliche Liste von normalverteilten Zufallswerten mit zugeh�igem
-- neuen Generator
allNormals :: RandomGen gen => gen -> Double -> [(Double, gen)]
allNormals gen sqrSigma =
  (z1, nextGen1) : (z2, nextGen2) : allNormals nextGen2 sqrSigma
  where
    z1 = sigma * sqrt (-2 * log x1') * cos (2 * pi * x2')
    z2 = sigma * sqrt (-2 * log x1') * sin (2 * pi * x2')
    sigma = sqrt sqrSigma

    x1' = if x1==0 then 1 else x1
    x2' = if x2==0 then 1 else x2
    (x1, nextGen1) = randomR (0, 1) gen
    (x2, nextGen2) = randomR (0, 1) nextGen1


-- terrainRandomFunc distrib n gen var
--
-- Erzeugt aus Generator gen eine Liste mit n Zufallswerten der Varianz var
-- und der Verteilungsart distrib, sowie einen neuen Generator.
--
terrainRandomFunc :: RandomGen gen => Distribution -> Int -> gen -> Double
  -> ([Double], gen)
terrainRandomFunc distrib n gen var = (map fst $ tail ls, nextGen)
  where
    (_, nextGen) = last ls
    ls = (0, gen) : (take n $ distribFunc gen var)
    distribFunc = case distrib of
      UniformDistribution -> allUniforms
      NormalDistribution  -> allNormals


-- neuner interpolierter H�enwert zwischen 4 H�enwerten und Zufallswert d
newHeight :: Double -> Double -> Double -> Double -> Double -> Double
newHeight h1 h2 h3 h4 d = (h1 + h2 + h3 + h4)/4 + d


-- Erzeugt aus einer Terrain-Ableitung und einem Zufallsgenerator
-- den n�hsten Ableitungsschritt und einen neuen Zufallsgenerator
nextTerrainStep :: RandomGen gen => ([[Double]], gen) -> ([[Double]], gen)
nextTerrainStep (hss, gen) = (hss', gen')
  where
    hss' = interleave
      (zipWith interleave hss squares1)
      (zipWith interleave squares2 diamonds)

    squares2 = zipWith4 squareStep
      hss (map lastAndInit diamonds) (tailAndHead hss)
      (dsSquares2 `inGroupsOf` nx)

    squares1 = zipWith4 squareStep
      (lastAndInit diamonds) hss diamonds (dsSquares1 `inGroupsOf` nx)

    diamonds = zipWith3 diamondStep
      hss (tailAndHead hss) (dsDiamonds `inGroupsOf` nx)

    [dsDiamonds, dsSquares1, dsSquares2] = dsAll `inGroupsOf` (nx*ny)
    (dsAll, gen') = terrainRandomFunc global_distribution (3*nx*ny) gen
      (variance (fromInteger n) global_roughness)

    n = toInteger $ logBase2 nx
    nx = length (head hss)
    ny = length hss

    -- Diamond-Schritt
    --  o   o   o      o   o   o <- uppers
    --             =>    x   x
    --  o   o   o      o   o   o <- lowers
    diamondStep :: [Double] -> [Double] -> [Double] -> [Double]
    diamondStep lowers uppers ds = zipWith5 newHeight
      lowers uppers (tailAndHead lowers) (tailAndHead uppers) ds

    -- Square-Schritt
    --    o   o          o   o   <- uppers
    --  o   o   o  =>  o x o x o <- centers
    --    o   o          o   o   <- lowers
    squareStep :: [Double] -> [Double] -> [Double] -> [Double] -> [Double]
    squareStep lowers centers uppers ds = zipWith5 newHeight
      lowers centers (tailAndHead centers) uppers ds


-- Unendliche Liste aller Ableitungen eines Terrains
allTerrainSteps gen = iterate nextTerrainStep ([[0]], gen)


-- Erweitert Terrain um den redundanten oberen und rechten Rand
enlargeTerrain :: [[Double]] -> [[Double]]
enlargeTerrain hss = map enlarge (enlarge hss)
  where
    enlarge = \cs -> if null cs then [] else cs ++ [head cs]
