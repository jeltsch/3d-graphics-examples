module Main (main) where

--------------------------------------------------------------------------------
--
-- Graphische Darstellung von Lindenmayer-Systemen
--
-- Tastenbelegungen im Hauptfenster:
-- Links, Rechts              -> Ansicht um Up-Achse drehen
-- Oben, Unten                -> Ansicht um Head-Achse drehen
-- Strg + Pfeiltaste          -> Ansicht verschieben
-- '+', '-'                   -> Ableitungsschritt �ndern
-- 'I', 'O'                   -> Hinein- bzw. Hinauszoomen
--
--------------------------------------------------------------------------------

import Graphics.UI.GLUT hiding (initState)
import Graphics.Rendering.OpenGL

import LSystem
import Turtle

import Utilities

import Data.Char (toUpper)
import Data.IORef

import KochLSystem     -- Koch'sche Schneeflocke, Pr�fix: "koch"
import IslandLSystem   -- Aufgabe 1.a, Pr�fix: "island"
import TreeLSystem     -- Aufgabe 1.b, Pr�fix: "tree"
import ConiferLSystem  -- Aufgabe 1.c, Pr�fix: "conifer"

-- Pr�fix f�r andere Beispiele �ndern
global_Interpretation = kochInterpretation
global_LSystem = kochLSystem


-- Aktueller Zustand des Systems
data State = State {
  derivationIndex :: Int,

  viewPhi :: Double,
  viewTheta :: Double,
  zoom :: Double,
  pan :: (Double, Double),
  viewRatio :: Double,
  renderingRequired :: Bool,

  currentLineWidth :: Double,
  lineWidthStack :: [Double]
  }


initState = State {
  derivationIndex = 0,

  viewPhi = 0,
  viewTheta = 0,
  zoom = 1,
  pan = (0,0),
  viewRatio =
    (fromIntegral global_windowSizeX) / (fromIntegral global_windowSizeY),
  renderingRequired = True,

  currentLineWidth = 1,
  lineWidthStack = []
  }


-- globale Werte
global_windowTitle = "Lindenmayer-Systeme"
global_windowSizeX = 400
global_windowSizeY = 400
global_pixelWidth = 2 / (fromIntegral global_windowSizeX)



-------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------


main = do
  getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]

  state <- newIORef initState

  depthFunc $= Just Less
  createWindow global_windowTitle
  windowSize $= Size global_windowSizeX global_windowSizeY

  lighting $= Enabled
  normalize $= Enabled
  depthFunc $= Just Less

  displayCallback $= display state
  keyboardMouseCallback $= Just (keyboard state)
  reshapeCallback $= Just (reshape state)

  mainLoop



-- Hauptzeichenfunktion --------------------------------------------------------


display state = do
  curState <- get state
  let
    z = realToReal $ zoom curState
    (panX, panY) = pan curState
    rat = viewRatio curState

  projection (-z) (z) (-z / realToReal rat) (z / realToReal rat) (-1000) (1000)

  clearColor $= Color4 0 0 0 0
  clear [ColorBuffer, DepthBuffer]

  loadIdentity

  position (Light 0) $= Vertex4 (-1) (1) 10 1
  ambient (Light 0) $= Color4 1 1 1 1
  diffuse (Light 0) $= Color4 1 1 1 1
  specular (Light 0) $= Color4 1 1 1 1
  light (Light 0) $= Enabled

  cCyanMaterial

  translate $ Vector3 (doubleToGLfloat panX) (doubleToGLfloat panY) 0

  rotate (doubleToGLfloat $ negate $ viewTheta curState) $ Vector3 1 0 0
  rotate (doubleToGLfloat $ viewPhi curState) $ Vector3 0 0 1

  let i = derivationIndex curState

  if (renderingRequired curState)
    then defineList (DisplayList 1) CompileAndExecute $ do
      sequence_ $ (render state $
        interpretations global_Interpretation $
        derivations global_LSystem) !! i

    else do
      callList (DisplayList 1)

  windowTitle $= global_windowTitle ++ " (n=" ++ (show i) ++ ")"
  state $= curState { renderingRequired = False }
  swapBuffers



-- Fensterskalierung -----------------------------------------------------------


reshape state s = do
  curState <- get state
  let (Size x y) = s
  state $= curState { viewRatio = (fromIntegral x) / (fromIntegral y) }
  viewport $= (Position 0 0, s)



-- Tastatur-Ereignisverarbeitung -----------------------------------------------


keyboard state (Char key) Down _ _ = do
  curState <- get state
  let
    i = derivationIndex curState
    z = zoom curState

  case toUpper key of
    'I' -> do
      state $= curState { zoom = z / 1.05 }
      postRedisplay Nothing
    'O' -> do
      state $= curState { zoom = z * 1.05 }
      postRedisplay Nothing
    '+' -> do
      state $= curState { derivationIndex = i+1, renderingRequired = True }
      postRedisplay Nothing
    '-' -> do
      state $= curState { derivationIndex = if (i==0) then 0 else i-1,
        renderingRequired = True }
      postRedisplay Nothing
    _ -> return ()

keyboard state (SpecialKey specialKey) Down mod _ = do
  curState <- get state
  let
    phi = viewPhi curState
    theta = viewTheta curState
    ctrlPressed = ctrl mod == Down
    z = zoom curState
    (x,y) = pan curState

  case specialKey of
    KeyLeft -> do
      if ctrlPressed
        then do
          state $= curState { pan = (x-0.05*z, y) }
        else do
          state $= curState { viewPhi = if phi < 0 then phi+355 else phi-5 }
      postRedisplay Nothing
    KeyRight -> do
      if ctrlPressed
        then do
          state $= curState { pan = (x+0.05*z, y) }
        else do
          state $= curState { viewPhi = if phi >= 360 then phi-355 else phi+5 }
      postRedisplay Nothing

    KeyDown -> do
      if ctrlPressed
        then do
          state $= curState { pan = (x, y-0.05*z) }
        else do
          state $= curState
            { viewTheta = if theta < 0 then theta+355 else theta-5 }
      postRedisplay Nothing
    KeyUp -> do
      if ctrlPressed
        then do
          state $= curState { pan = (x, y+0.05*z) }
        else do
          state $= curState
            { viewTheta = if theta >= 360 then theta-355 else theta+5 }
      postRedisplay Nothing
    _ -> return ()

keyboard _ _ _ _ _ = return ()



-- Grafik-Berechnung -----------------------------------------------------------


-- Drehung um alpha Grad um die Up-Achse
rotateU :: GLfloat -> IO ()
rotateU alpha = rotate alpha $ Vector3 0 0 (1::GLfloat)

turnLeft = rotateU
turnRight = rotateU . negate
turnAround = rotateU 180


-- Drehung um alpha Grad um die Left-Achse
rotateL :: GLfloat -> IO ()
rotateL alpha = rotate alpha $ Vector3 0 (1) (0::GLfloat)

pitchDown = rotateL
pitchUp = rotateL . negate


-- Drehung um alpha Grad um die Heading-Achse
rotateH :: GLfloat -> IO ()
rotateH alpha = rotate alpha $ Vector3 (-1) 0 (0::GLfloat)

rollLeft = rotateH
rollRight = rotateH . negate


-- forward state length draw
--
-- Turtle length Pixel vorw�rts bewegen. Ist draw = True wird ein Zylinder
-- der L�nge length im Raum gezeichnet, sonst nur die Position ver�ndert.
--
forward :: IORef State -> GLfloat -> Bool -> IO ()
forward state length draw = do
  curState <- get state

  let
    len = global_pixelWidth * length
    wid = global_pixelWidth * realToReal (currentLineWidth curState)
    r = wid/2
    d = global_pixelWidth

  if draw
    then do
      if len/=0 then drawCylinder r r len 32 else return ()
      translate $ Vector3 len 0 0
      renderObject Solid $
        Sphere' (realToReal $ r + (global_pixelWidth/10)) 8 8
    else do
      translate $ Vector3 len 0 0


-- Liste von TurtleModul-Sequenzen in Zeichenfunktionen �bersetzen
render :: IORef State -> [ [LPrim TurtleModule] ] -> [ [IO ()] ]
render state = map render'
  where
    render' = map r
    r (LStack s) = do
      curState <- get state
      let
        oldStack = lineWidthStack curState
        wid = currentLineWidth curState
      state $= curState { lineWidthStack = wid:oldStack }
      preservingMatrix $ sequence_ $ render' s
      state $= curState { lineWidthStack = oldStack }

    r (LPrim p) = case p of
      TDraw len         -> forward state (realToReal len) True
      TMove len         -> forward state (realToReal len) False

      TTurnLeft alpha   -> turnLeft $ realToReal alpha
      TTurnRight alpha  -> turnRight $ realToReal alpha
      TTurnAround       -> turnAround

      TPitchDown alpha  -> pitchDown $ realToReal alpha
      TPitchUp alpha    -> pitchUp $ realToReal alpha

      TRollLeft alpha   -> rollLeft $ realToReal alpha
      TRollRight alpha  -> rollRight $ realToReal alpha

      TSetLineWidth wid -> do
        curState <- get state
        state $= curState { currentLineWidth = realToReal wid }


-- Zeichnet Zylinder in der y-z-Ebene (= Turtle-R�cken-Ebene)
drawCylinder ry rz len n = do
  renderPrimitive QuadStrip (vertexesN $ interleave fullE fullE')
  where
    fullE' = map (\(x,y,z) -> (x+len,y,z)) fullE
    fullE = [(0,y,z::GLfloat) | i<-[0..n],
      let a = 2 * pi * i/n, let y = -ry * cos a, let z = rz * sin a]


-- Gibt eine Liste von Punkten in Form von vertex-Befehlen wieder. Die Liste
-- wird als QuadStrip interpretiert und f�r jedes Rechteck der entsprechende
-- Normalenvektor eingef�gt
vertexesN :: (VertexComponent a, NormalComponent a, Num a) => [(a,a,a)] -> IO ()
vertexesN (a:b:c:d:rs) = do
  normal $ norm a b c
  vertex $ vert a
  vertex $ vert b
  vertexesN (c:d:rs)
  where
    vert = \(x,y,z) -> Vertex3 x y z
    norm = \v1 v2 v3 -> let (x, y, z) = (v2-v1) * (v3-v1) in Normal3 x y z
-- Restliche Punkte einfach in Vertexes umwandeln
vertexesN rs = sequence_ $ map (\(x,y,z) -> vertex (Vertex3 x y z)) rs
