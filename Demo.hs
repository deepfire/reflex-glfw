---
--- This engages Reflex.GLFW for a reinterpretation of https://github.com/bsl/GLFW-b-demo/blob/master/src/Main.hs
--- which is the source for all of the non-FRP code of the demo.
---
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main (main) where

-- TODO:
--  1. wire in event printing
--  2. close the XXX's
--

import Control.Arrow             ((***))
import Control.Concurrent.STM    (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import Control.Lens
import Control.Monad             (join, unless, void, when, forM_)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.RWS.Strict  (RWST, ask, asks, evalRWST, get, liftIO, modify, put)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.Bool
import Data.Function
import Data.List                 (intercalate)
import Data.Maybe                (catMaybes, isJust)
import Data.Semigroup
import Prelude.Unicode
import Text.PrettyPrint          hiding ((<>))

import qualified Graphics.Rendering.OpenGL         as GL
import qualified Graphics.UI.GLFW                  as GLFW
import qualified System.IO                         as Sys

import           Reflex
import           Reflex.GLFW                       as RGL


data Env = Env
    { envGear1         ∷ !GL.DisplayList
    , envGear2         ∷ !GL.DisplayList
    , envGear3         ∷ !GL.DisplayList
    , envZDistClosest  ∷ !Double
    , envZDistInitial  ∷ !Double
    , envZDistFarthest ∷ !Double }

guestInit ∷ (MonadIO m) ⇒ GLFW.Window → m Env
guestInit win = do
    liftIO $ GLFW.swapInterval 1

    GL.position (GL.Light 0) GL.$= GL.Vertex4 5 5 10 0
    GL.light    (GL.Light 0) GL.$= GL.Enabled
    GL.lighting   GL.$= GL.Enabled
    GL.cullFace   GL.$= Just GL.Back
    GL.depthFunc  GL.$= Just GL.Less
    GL.clearColor GL.$= GL.Color4 0.05 0.05 0.05 1
    GL.normalize  GL.$= GL.Enabled

    gear1 ← liftIO $ makeGear 1   4 1   20 0.7 (GL.Color4 0.8 0.1 0   1)  -- red
    gear2 ← liftIO $ makeGear 0.5 2 2   10 0.7 (GL.Color4 0   0.8 0.2 1)  -- green
    gear3 ← liftIO $ makeGear 1.3 2 0.5 10 0.7 (GL.Color4 0.2 0.2 1   1)  -- blue

    let zDistClosest  = 10
        zDistFarthest = zDistClosest + 20
        env = Env
          { envGear1         = gear1
          , envGear2         = gear2
          , envGear3         = gear3
          , envZDistClosest  = zDistClosest
          , envZDistInitial  = zDistClosest + ((zDistFarthest - zDistClosest) / 2)
          , envZDistFarthest = zDistFarthest
          }
    pure env

updateView ∷ RGLFW t m
           ⇒ Event t (Double, GLFW.Window)
           → m (Event t ())
updateView newFrameE = performEvent $ newFrameE <&>
  \(zDist, win) → do
    (width, height) ← liftIO $ GLFW.getFramebufferSize win

    let pos   = GL.Position 0 0
        size  = GL.Size (fromIntegral width) (fromIntegral height)
        h     = fromIntegral height / fromIntegral width ∷ Double
        znear = 1           ∷ Double
        zfar  = 40          ∷ Double
        xmax  = znear * 0.5 ∷ Double
    liftIO $ do
        GL.viewport   GL.$= (pos, size)
        GL.matrixMode GL.$= GL.Projection
        GL.loadIdentity
        GL.frustum (realToFrac $ -xmax)
                   (realToFrac    xmax)
                   (realToFrac $ -xmax * realToFrac h)
                   (realToFrac $  xmax * realToFrac h)
                   (realToFrac    znear)
                   (realToFrac    zfar)
        GL.matrixMode GL.$= GL.Modelview 0
        GL.loadIdentity
        GL.translate (GL.Vector3 0 0 (negate $ realToFrac zDist) ∷ GL.Vector3 GL.GLfloat)

draw ∷ RGLFW t m
     ⇒ Env → Event t ((Double, Double, Double), Double)
     → m (Event t ())
draw Env{..} statE = performEvent $ statE <&>
  \((xa, ya, za), ga) → do
    let gear1 = envGear1
        gear2 = envGear2
        gear3 = envGear3
    liftIO $ do
        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
        GL.preservingMatrix $ do
            GL.rotate (realToFrac xa) xunit
            GL.rotate (realToFrac ya) yunit
            GL.rotate (realToFrac za) zunit
            GL.preservingMatrix $ do
                GL.translate gear1vec
                GL.rotate (realToFrac ga) zunit
                GL.callList gear1
            GL.preservingMatrix $ do
                GL.translate gear2vec
                GL.rotate (-2 * realToFrac ga - 9) zunit
                GL.callList gear2
            GL.preservingMatrix $ do
                GL.translate gear3vec
                GL.rotate (-2 * realToFrac ga - 25) zunit
                GL.callList gear3
      where
        gear1vec = GL.Vector3 (-3)   (-2)  0 ∷ GL.Vector3 GL.GLfloat
        gear2vec = GL.Vector3   3.1  (-2)  0 ∷ GL.Vector3 GL.GLfloat
        gear3vec = GL.Vector3 (-3.1)   4.2 0 ∷ GL.Vector3 GL.GLfloat
        xunit = GL.Vector3 1 0 0 ∷ GL.Vector3 GL.GLfloat
        yunit = GL.Vector3 0 1 0 ∷ GL.Vector3 GL.GLfloat
        zunit = GL.Vector3 0 0 1 ∷ GL.Vector3 GL.GLfloat

nextFrame ∷ RGLFW t m
          ⇒ GLFW.Window → Event t ()
          → m (Event t ())
nextFrame win windowFrameE = performEvent $ windowFrameE <&>
  \_ → liftIO $ do
    GLFW.swapBuffers win
    GL.flush  -- not necessary, but someone recommended it
    GLFW.pollEvents

newtype PointerDrag = PointerDrag ((Double, Double), (Double, Double))

demoGuest ∷ RGLFWGuest t m
demoGuest win _ec windowFrameE inputE = do
  liftIO $ Sys.hSetBuffering Sys.stdout Sys.NoBuffering
  env@Env{..} ← guestInit win
  printInstructions

  _ ← getPostBuild

  mTimeE            ← performEvent $ (const $ liftIO GLFW.getTime) <$> windowFrameE
  let timeE         = fmapMaybe id mTimeE

  -- Input event sources, cagegorized
  let keyE          = filterKey                      inputE
      buttonE       = filterMouseButton              inputE
      pointerE      = filterCursorPos                inputE
      errorE        = filterError                    inputE
      scrollE       = filterScroll                   inputE
  joystickE         ← sampleJoystick GLFW.Joystick'1 inputE

  -- Processed input events, tier I
  mousePressD       ← mouseButtonState GLFW.MouseButton'1                  buttonE
  pointerD          ← holdDyn (0, 0)                 $ cursorPosCoords <$> pointerE
  arrowDirectD      ← arrowKeyDirections                                   keyE
  joystickDirectD   ← joystickDirections                                   joystickE
  let quitRequestE  = (() <$ filterKeyPress GLFW.Key'Q                     keyE) <>
                      (() <$ filterKeyPress GLFW.Key'Escape                keyE)
      infoRequestE  =  () <$ filterKeyPress GLFW.Key'I                     keyE
      helpRequestE  =  () <$ filterKeyPress GLFW.Key'Slash                 keyE --XXX: GLFW.modifierKeysShift mk

  performEvent $ filterKeyPress GLFW.Key'J keyE <&>
    (const $ liftIO $ enableEvent _ec MouseButton)
  performEvent $ filterKeyPress GLFW.Key'K keyE <&>
    (const $ liftIO $ disableEvent _ec MouseButton)

  -- ..tier II
  let keyJoyDirectD = join (***) (*2) <$> zipDynWith (\(kxrot , kyrot) (jxrot , jyrot)→
                                                       (kxrot + jxrot , kyrot + jyrot))
                                            arrowDirectD   joystickDirectD
  -- World data
  gearZAngleD       ← holdDyn 0 $ realToFrac ∘ (100*) <$> timeE
  zDistD            ← flip foldDyn envZDistInitial
                      (\scrY zDist →
                         let zDist' = zDist + realToFrac (negate $ scrY / 2)
                         in curb envZDistClosest envZDistFarthest zDist')
                      (scrollY <$> scrollE)

  -- | Angles computation:
  -- need:  Event t (xangle, yangle, zangle)
  -- Original logig goes:
  --   non-mouse-drag case:
  --    - from samples of arrowkeys and joystick:   -- roughly keyJoyDirectD
  --    -   to: (xangle, yangle) + (arrjoydx, arrjoydy) fold from (0, 0)
  --   mouse drag case:
  --    - from:
  --      1. (x, y) current sample of cursor,       -- pointerD
  --      2. (sodx, sody) sample of cursor,
  --            frozen at Start-Of-Drag             -- mousePressD
  --      3. (sodXAngle, sodYAngle) state,
  --            frozen at Start-Of-Drag             -- anglesD' recursive value
  --    -   to: (xangle, yangle) direct computation as
  --            (sodxangle, sodyangle) + (f (x, y) (sodx, sody)
  rec anglesD' ← do
        soD ← flip foldDyn Nothing (\(sodAngles, mSOD) _old →
                                      mSOD <&> (sodAngles,))
              $ attach (current anglesD') (updated mousePressD)
        let compositE = (() <$ updated pointerD) <>
                        (() <$ updated keyJoyDirectD)
                        & attachPromptlyDyn pointerD
                        & attachPromptlyDyn keyJoyDirectD
                        & attachPromptlyDyn soD
        flip foldDyn (0, 0, 0)
             (\(mDrag, ((kjXAngle, kjYAngle), ((x, y), ())))
               (xAngle, yAngle, zAngle) → -- The Z angle stays intact at 0,
                case mDrag of             -- in the original demo.
                  Nothing     →
                    (xAngle + kjXAngle, yAngle + kjYAngle, zAngle)
                  Just ((sodXA, sodYA, sodZA), (sodX, sodY)) →
                    let xrot = (x - sodX) / 2
                        yrot = (y - sodY) / 2
                    in (sodXA + xrot, sodYA + yrot, sodZA))
             compositE

  let worldD = zipDyn anglesD' gearZAngleD

  -- Rendering: these pass a token ()-Event to facilitate ordering.
  viewUpdatedE ← updateView $ attachPromptlyDyn zDistD windowFrameE
  (draw env $ fst <$> attachPromptlyDyn worldD viewUpdatedE)
   >>= nextFrame win

  -- Effects
  performEvent $ infoRequestE <&>
    (const $ liftIO $ printInformation win)
  performEvent $ helpRequestE <&>
    (const $ liftIO $ printInstructions)
  --- XXX: print error on error
  performEvent $ quitRequestE <> (() <$ errorE) <&>
    (const $ liftIO $ GLFW.setWindowShouldClose win True)

  hold False =<< (performEvent $ (const $ liftIO $ GLFW.windowShouldClose win) <$> inputE)

main ∷ IO ()
main = do
  success ← RGL.tryInit
  unless success $
    error "GLFW failed to initialise GL."

  -- No special GL.windowHint -ing is required for this demo, so just proceeding as-is.
  RGL.withGLWindow 640 480 "reflex-glfw-demo" $
    \win → do
      RGL.host win demoGuest
      putStrLn "ended!"


processEvent ∷ RGLFW t m ⇒ Event t InputU → m (Event t ())
processEvent inputE = performEvent $ inputE <&>
  \case
    U (EventError e s) → liftIO $ do
        printEvent "error" [show e, show s]
    U (EventWindowPos _ x y) →
        printEvent "window pos" [show x, show y]
    U (EventWindowSize _ width height) →
        printEvent "window size" [show width, show height]
    U (EventWindowClose _) →
        printEvent "window close" []
    U (EventWindowRefresh _) →
        printEvent "window refresh" []
    U (EventWindowFocus _ fs) →
        printEvent "window focus" [show fs]
    U (EventWindowIconify _ is) →
        printEvent "window iconify" [show is]
    U (EventFramebufferSize _ width height) → do
        printEvent "framebuffer size" [show width, show height]
    U (EventMouseButton _ mb mbs mk) → do
        printEvent "mouse button" [show mb, show mbs, Main.showModifierKeys mk]
    U (EventCursorPos _ x y) → do
        let x' = round x ∷ Int
            y' = round y ∷ Int
        printEvent "cursor pos" [show x', show y']
    U (EventCursorEnter _ cs) →
        printEvent "cursor enter" [show cs]
    U (EventScroll _ x y) → do
        let x' = round x ∷ Int
            y' = round y ∷ Int
        printEvent "scroll" [show x', show y']
    U (EventKey win k scancode ks mk) → do
        printEvent "key" [show k, show scancode, show ks, Main.showModifierKeys mk]
    U (EventChar _ c) →
        printEvent "char" [show c]

arrowKeyDirections ∷ RGLFW t m
                   ⇒ Event t (Input Key)
                   → m (Dynamic t (Double, Double))
arrowKeyDirections inputE = do
  x0 ← (bool 0 (-1) <$>) <$> keyState GLFW.Key'Up    inputE
  x1 ← (bool 0  (1) <$>) <$> keyState GLFW.Key'Down  inputE
  y0 ← (bool 0 (-1) <$>) <$> keyState GLFW.Key'Left  inputE
  y1 ← (bool 0  (1) <$>) <$> keyState GLFW.Key'Right inputE
  let x = zipDynWith (+) x0 x1
      y = zipDynWith (+) y0 y1
  pure $ zipDynWith (,) x y

joystickDirections ∷ RGLFW t m
                   ⇒ Event t JoystickSample
                   → m (Dynamic t (Double, Double))
joystickDirections jsE =
  holdDyn (0, 0) =<< (performEvent $ jsE <&>
                       \(JoystickSample (x:y:_)) → pure (-y, x))
                       -- \(JoystickSample _) → pure (0, 0))


printInstructions ∷ (MonadIO m) ⇒ m ()
printInstructions =
    liftIO $ putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "'?': Print these instructions"                                $+$
        text "'i': Print GLFW information"                                  $+$
        text ""                                                             $+$
        text "* Mouse cursor, keyboard cursor keys, and/or joystick"        $+$
        text "  control rotation."                                          $+$
        text "* Mouse scroll wheel controls distance from scene."           $+$
        text ""                                                             $+$
        text "'j'/'k': Enable/disable mouse button sensitivity."            $+$
        text "------------------------------------------------------------"
      )

printInformation ∷ (MonadIO m) ⇒ GLFW.Window → m ()
printInformation win = liftIO $ do
    version       ← GLFW.getVersion
    versionString ← GLFW.getVersionString
    monitorInfos  ← runMaybeT getMonitorInfos
    joystickNames ← getJoystickNames
    clientAPI     ← GLFW.getWindowClientAPI              win
    cv0           ← GLFW.getWindowContextVersionMajor    win
    cv1           ← GLFW.getWindowContextVersionMinor    win
    cv2           ← GLFW.getWindowContextVersionRevision win
    robustness    ← GLFW.getWindowContextRobustness      win
    forwardCompat ← GLFW.getWindowOpenGLForwardCompat    win
    debug         ← GLFW.getWindowOpenGLDebugContext     win
    profile       ← GLFW.getWindowOpenGLProfile          win

    putStrLn $ render $
      nest 4 (
        text "------------------------------------------------------------" $+$
        text "GLFW C library:" $+$
        nest 4 (
          text "Version:"        <+> renderVersion version $+$
          text "Version string:" <+> renderVersionString versionString
        ) $+$
        text "Monitors:" $+$
        nest 4 (
          renderMonitorInfos monitorInfos
        ) $+$
        text "Joysticks:" $+$
        nest 4 (
          renderJoystickNames joystickNames
        ) $+$
        text "OpenGL context:" $+$
        nest 4 (
          text "Client API:"            <+> renderClientAPI clientAPI $+$
          text "Version:"               <+> renderContextVersion cv0 cv1 cv2 $+$
          text "Robustness:"            <+> renderContextRobustness robustness $+$
          text "Forward compatibility:" <+> renderForwardCompat forwardCompat $+$
          text "Debug:"                 <+> renderDebug debug $+$
          text "Profile:"               <+> renderProfile profile
        ) $+$
        text "------------------------------------------------------------"
      )
  where
    renderVersion (GLFW.Version v0 v1 v2) =
        text $ intercalate "." $ map show [v0, v1, v2]

    renderVersionString =
        text . show

    renderMonitorInfos =
        maybe (text "(error)") (vcat . map renderMonitorInfo)

    renderMonitorInfo (name, (x,y), (w,h), vms) =
        text (show name) $+$
        nest 4 (
          location <+> size $+$
          fsep (map renderVideoMode vms)
        )
      where
        location = int x <> text "," <> int y
        size     = int w <> text "x" <> int h <> text "mm"

    renderVideoMode (GLFW.VideoMode w h r g b rr) =
        brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz  = int rr <> text "Hz"

    renderJoystickNames pairs =
        vcat $ map (\(js, name) → text (show js) <+> text (show name)) pairs

    renderContextVersion v0 v1 v2 =
        hcat [int v0, text ".", int v1, text ".", int v2]

    renderClientAPI         = text . show
    renderContextRobustness = text . show
    renderForwardCompat     = text . show
    renderDebug             = text . show
    renderProfile           = text . show

type MonitorInfo = (String, (Int,Int), (Int,Int), [GLFW.VideoMode])

getMonitorInfos ∷ MaybeT IO [MonitorInfo]
getMonitorInfos =
    getMonitors >>= mapM getMonitorInfo
  where
    getMonitors ∷ MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors

    getMonitorInfo ∷ GLFW.Monitor → MaybeT IO MonitorInfo
    getMonitorInfo mon = do
        name ← getMonitorName mon
        vms  ← getVideoModes mon
        MaybeT $ do
            pos  ← liftIO $ GLFW.getMonitorPos mon
            size ← liftIO $ GLFW.getMonitorPhysicalSize mon
            return $ Just (name, pos, size, vms)

    getMonitorName ∷ GLFW.Monitor → MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon

    getVideoModes ∷ GLFW.Monitor → MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon

getJoystickNames ∷ IO [(GLFW.Joystick, String)]
getJoystickNames =
    catMaybes `fmap` mapM getJoystick joysticks
  where
    getJoystick js =
        fmap (maybe Nothing (\name → Just (js, name)))
             (GLFW.getJoystickName js)


printEvent ∷ (MonadIO m) ⇒ String → [String] → m ()
printEvent cbname fields =
    liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

showModifierKeys ∷ GLFW.ModifierKeys → String
showModifierKeys mk =
    "[mod keys: " ++ keys ++ "]"
  where
    keys = if null xs then "none" else unwords xs
    xs = catMaybes ys
    ys = [ if GLFW.modifierKeysShift   mk then Just "shift"   else Nothing
         , if GLFW.modifierKeysControl mk then Just "control" else Nothing
         , if GLFW.modifierKeysAlt     mk then Just "alt"     else Nothing
         , if GLFW.modifierKeysSuper   mk then Just "super"   else Nothing
         ]

curb ∷ Ord a ⇒ a → a → a → a
curb l h x
  | x < l     = l
  | x > h     = h
  | otherwise = x


joysticks ∷ [GLFW.Joystick]
joysticks =
  [ GLFW.Joystick'1
  , GLFW.Joystick'2
  , GLFW.Joystick'3
  , GLFW.Joystick'4
  , GLFW.Joystick'5
  , GLFW.Joystick'6
  , GLFW.Joystick'7
  , GLFW.Joystick'8
  , GLFW.Joystick'9
  , GLFW.Joystick'10
  , GLFW.Joystick'11
  , GLFW.Joystick'12
  , GLFW.Joystick'13
  , GLFW.Joystick'14
  , GLFW.Joystick'15
  , GLFW.Joystick'16
  ]


makeGear ∷ Float → Float → Float → Int → Float → GL.Color4 GL.GLfloat → IO GL.DisplayList
makeGear inradius outradius width teeth toothdepth color =
    GL.defineNewList GL.Compile $ do
        GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
        GL.color color
        drawGear
  where
    drawGear = do
        let r0   = inradius
            r1   = outradius - toothdepth / 2
            r2   = outradius + toothdepth / 2
            pi2  = 2 * pi
            da   = pi2 / realToFrac teeth / 4 ∷ Float
            wd2  = width * 0.5
            wd2n = negate wd2

            calca ∷ Int → Float
            calca i = realToFrac i * pi2 / realToFrac teeth ∷ Float

        GL.shadeModel GL.$= GL.Flat
        normal 0 0 1

        -- front face
        GL.renderPrimitive GL.QuadStrip $
            forM_ [0 .. teeth] $ \i → do
                let a = calca i
                vertex (r0 * cos a) (r0 * sin a) wd2
                vertex (r1 * cos a) (r1 * sin a) wd2
                when (i < teeth) $ do
                    vertex (r0 * cos a) (r0 * sin a) wd2
                    vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2

        -- front sides of teeth
        GL.renderPrimitive GL.Quads $
            forM_ [0 .. pred teeth] $ \i → do
                let a = calca i
                vertex (r1 * cos a) (r1 * sin a) wd2
                vertex (r2 * cos (a + da)) (r2 * sin (a + da)) wd2
                vertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) wd2
                vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2

        normal 0 0 (-1)

        -- back face
        GL.renderPrimitive GL.QuadStrip $
            forM_ [0 .. teeth] $ \i → do
                let a = calca i
                vertex (r1 * cos a) (r1 * sin a) wd2n
                vertex (r0 * cos a) (r0 * sin a) wd2n
                when (i < teeth) $ do
                    vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2n
                    vertex (r0 * cos a) (r0 * sin a) wd2n

        -- back sides of teeth
        GL.renderPrimitive GL.Quads $
            forM_ [0 .. pred teeth] $ \i → do
                let a = calca i
                vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2n
                vertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) wd2n
                vertex (r2 * cos (a + da)) (r2 * sin (a + da)) wd2n
                vertex (r1 * cos a) (r1 * sin a) wd2n

        -- outward faces of teeth
        GL.renderPrimitive GL.QuadStrip $ do
            forM_ [0 .. pred teeth] $ \i → do
                let a = calca i
                vertex (r1 * cos a) (r1 * sin a) wd2
                vertex (r1 * cos a) (r1 * sin a) wd2n
                let u = r2 * cos (a + da) - r1 * cos a
                    v = r2 * sin (a + da) - r1 * sin a
                    len = sqrt (u * u + v * v)
                    un = u / len
                    vn = v / len
                normal vn (-un) 0
                vertex (r2 * cos (a + da)) (r2 * sin (a + da)) wd2
                vertex (r2 * cos (a + da)) (r2 * sin (a + da)) wd2n
                normal (cos a) (sin a) 0
                vertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) wd2
                vertex (r2 * cos (a + 2 * da)) (r2 * sin (a + 2 * da)) wd2n
                let u' = r1 * cos (a + 3 * da) - r2 * cos (a + 2 * da)
                    v' = r1 * sin (a + 3 * da) - r2 * sin (a + 2 * da)
                normal v' (-u') 0
                vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2
                vertex (r1 * cos (a + 3 * da)) (r1 * sin (a + 3 * da)) wd2n
                normal (cos a) (sin a) 0

            vertex (r1 * cos 0) (r1 * sin 0) wd2
            vertex (r1 * cos 0) (r1 * sin 0) wd2n

        GL.shadeModel GL.$= GL.Smooth

        -- inside radius cylinder
        GL.renderPrimitive GL.QuadStrip $
            forM_ [0 .. teeth] $ \i → do
                let a = calca i
                normal (negate $ cos a) (negate $ sin a) 0
                vertex (r0 * cos a) (r0 * sin a) wd2n
                vertex (r0 * cos a) (r0 * sin a) wd2

vertex ∷ Float → Float → Float → IO ()
vertex x y z =
    GL.vertex $ mkVertex x y z

mkVertex ∷ Float → Float → Float → GL.Vertex3 GL.GLfloat
mkVertex x y z =
    GL.Vertex3 (realToFrac x) (realToFrac y) (realToFrac z)

normal ∷ Float → Float → Float → IO ()
normal x y z =
    GL.normal $ mkNormal x y z

mkNormal ∷ Float → Float → Float → GL.Normal3 GL.GLfloat
mkNormal x y z =
    GL.Normal3 (realToFrac x) (realToFrac y) (realToFrac z)
