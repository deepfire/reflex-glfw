{-|
Module      : Reflex.GLFW
Description : A GLFW-b adapter for the Haskell Reflex FRP implementation.
Copyright   : (c) Serge Kosyrev, 2017
License     : BSD-3
Maintainer  : _deepfire@feelingofgreen.ru (temporarily defunct)
Stability   : experimental
Portability : Unspecified

-}
{-# OPTIONS_GHC -Wall -Wno-unused-do-bind -Wno-unused-top-binds -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnicodeSyntax #-}
module Reflex.GLFW
  ( ReflexGLFW, ReflexGLFWCtx, ReflexGLFWM, ReflexGLFWGuest
  -- * GL window setup
  , tryInit, init
  , gl33ForwardCoreSetup
  , withGLWindow, defaultGLWindowSetup
  -- * Non-event-driven input sampling
  , JoystickSample(..), sampleJoystick
  , PointerSample(..), samplePointer, pointerSampleZero
  , mouseButtonStateIsPress, keyStateIsPress
  , cursorPosCoords
  , scrollX, scrollY
  -- * Input Events
  , EventType(..), Input(..), InputU(..)
  , EventMask(..), ButtonEventMask(..), KeyEventMask(..)
  , eventType, eventUType, eventMatch, eventMaskKeys, eventMaskButtons, eventMaskChars
  , eventTypeMaskTest, eventMaskTypes
  , mappendModifierKeys, subsetModifierKeys
  , EventCtl, setEvent, enableEvent, disableEvent
  , filterError
  , filterWindowRefresh
  , filterFramebufferSize
  , filterScroll
  , filterKey, filterKey', filterKeyPress
  , filterMouseButton, filterMouseButton', filterMouseButtonStateChange
  , filterCursorPos
  , filterChar
  -- * Input Dynamics
  , mouseButtonState
  , keyState
  -- * Reflex hosts
  , simpleHost
  , basicHost, basicGL33Host
  , host
  )
where

import           GHC.Stack
import           Prelude                            hiding (Char, init)
import qualified Prelude                            as Prelude
import           Prelude.Unicode

import           Control.Concurrent                        (forkIO, threadDelay)
import           Control.Concurrent.Chan                   (newChan, readChan)
import qualified Control.Concurrent.STM             as STM (TQueue, atomically, newTQueueIO, tryReadTQueue, writeTQueue)
import           Control.Lens
import           Control.Monad                             (unless, forever, forM_, forM, void)
import           Control.Monad.Fix                         (MonadFix)
import           Control.Monad.Identity                    (Identity(..))
import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           Control.Monad.Primitive                   (PrimMonad)
import           Control.Monad.Ref                         (MonadRef(..))
import           Data.Dependent.Sum                        (DSum ((:=>)))
import           Data.IORef                                (readIORef)
import           Data.List                                 (intercalate)
import qualified Data.Map                           as Map
import           Data.Map                                  (Map)
import           Data.Maybe
import qualified Data.Set                           as Set
import           Data.Set                                  (Set)
import           Data.Typeable

import qualified Graphics.GL.Core33                 as GL
import qualified "GLFW-b" Graphics.UI.GLFW          as GL
import qualified System.IO                          as Sys

import           Reflex
import           Reflex.Host.Class

import           Reflex.GLFW.Instances


-- | The constructor for Reflex FRP networks runnable by 'Reflex.GLFW' hosts is a
-- function, that takes:
--
--       * the GLFW window object
--       * an event control object (currently used for muting/unmuting categories)
--       * the setup event, that fires just once at beginning
--       * stream of new-frame events
--       * stream of the enabled subset of GLFW input events
--
--   ..and produces an output boolean behavior in the 'ReflexGLFWCtx'-constrained
--     monad, that is interpreted as a request for termination -- shutdown once
--     'True'.
--
type ReflexGLFWCtx t m =
  ( MonadReflexHost t m
  , MonadHold t m
  , Ref m ~ Ref IO
  , MonadRef (HostFrame t)
  , Ref (HostFrame t) ~ Ref IO
  , MonadIO (HostFrame t)
  , PrimMonad (HostFrame t)
  , Reflex t
  , MonadRef m
  , MonadIO m
  , MonadFix m
  , Typeable m
  )

type ReflexGLFWM   t m   = PostBuildT t (TriggerEventT t (PerformEventT t m))

type ReflexGLFW    t m a =
     ReflexGLFWCtx t m   ⇒ PostBuildT t (TriggerEventT t (PerformEventT t m)) a

type ReflexGLFWGuest t m =
    (ReflexGLFWCtx   t m)
  ⇒ GL.Window                    -- ^ The GLFW window to draw unto and suck events from
  → EventCtl                     -- ^ An "event control" handler, providing IO actions to
                                 --   mute/unmute events at GLFW level with 'EventType'
                                 --   granularity: see 'setEvent'
  → Event t GL.Window            -- ^ The window to draw on, fired on every frame
  → Event t InputU               -- ^ Fired whenever input happens, which isn't always the case..
                                 --
                                 -- → The monadic-FRP-network-builder to be passed to 'host'
  → ReflexGLFW t m (Behavior t Bool)


-- * GLFW error handling
--
errormsgIO ∷ String → IO ()
errormsgIO = Sys.hPutStrLn Sys.stderr

errormsg ∷ (MonadIO m) ⇒ String → m ()
errormsg = liftIO ∘ errormsgIO

simpleErrorPrinter ∷ GL.ErrorCallback
simpleErrorPrinter e s =
  errormsg $ unwords [show e, show s]


-- * GL window setup
--
-- | This function initialises 'Reflex.GLFW' and returns 'False' upon failure.
--
--   If it succeeds, we might follow with setting GL window hints as/if needed,
--   using regular 'GLFW-b' API, like 'GLFW.windowHint'.
--
--   Then, proceed with running our FRP network using a call to 'host', nested
--   inside the 'withGLWindow' context bracket.
tryInit ∷ (MonadIO m) ⇒ m Bool
tryInit = liftIO $ do
  GL.setErrorCallback $ Just simpleErrorPrinter
  GL.init

-- | Like 'init' but raise an error, instead of returning 'False'.
init ∷ (HasCallStack, MonadIO io) ⇒ io ()
init = do
  success ← tryInit
  unless success $
    error "GLFW failed to initialise GL."
  pure ()

-- | Request a forward-compatible OpenGL 3.3 core profile.
--   Should be called between 'init' / 'tryInit' and 'withGLWindow'.
gl33ForwardCoreSetup ∷ (MonadIO m) ⇒ m ()
gl33ForwardCoreSetup = liftIO $ do
  GL.defaultWindowHints
  mapM_ GL.windowHint
    [ GL.WindowHint'ContextVersionMajor 3
    , GL.WindowHint'ContextVersionMinor 3
    , GL.WindowHint'OpenGLProfile GL.OpenGLProfile'Core
    , GL.WindowHint'OpenGLForwardCompat True ]

-- | Call this function after 'init' / 'tryInit', and once the necessary GLFW
-- window hints are provided (if any were deemed required, that is) either by
-- 'gl33ForwardCoreSetup' or by direct "GLFW" API calls.

withGLWindow ∷ (MonadIO m) ⇒ Int → Int → String → (GL.Window → m a) → m a
withGLWindow width height title f = do
    liftIO $ GL.setErrorCallback $ Just simpleErrorPrinter
    m ← liftIO $ GL.createWindow width height title Nothing Nothing
    r ← case m of
          Just win → do
            liftIO $ GL.makeContextCurrent m
            r ← f win
            liftIO $ GL.setErrorCallback $ Just simpleErrorPrinter
            liftIO $ GL.destroyWindow win
            pure r
          Nothing → do
            let msg = "Failed to create a GL window.  Was 'init' called?  Were the requested window hints (if any) appropriate?"
            errormsg msg
            error    msg

    liftIO $ GL.terminate
    pure r

-- | Setup a GLFW window according to a certain notion of "default".
defaultGLWindowSetup ∷ (MonadIO m) ⇒ GL.Window → m ()
defaultGLWindowSetup _ = liftIO $ do
  GL.glEnable GL.GL_FRAMEBUFFER_SRGB
  GL.swapInterval 0


-- * Non-event-driven input sampling
newtype JoystickSample = JoystickSample { jSample ∷ [Double] }
newtype PointerSample  = PointerSample  { pSample ∷ (Double, Double) }

pointerSampleZero ∷ PointerSample
pointerSampleZero = PointerSample (0, 0)

sampleJoystick ∷ ReflexGLFWCtx t m ⇒ GL.Joystick → Event t a → ReflexGLFW t m (Event t JoystickSample)
sampleJoystick js ev = do
  e ← fmapMaybe id <$> (performEvent $ ev <&> (const $ liftIO $ GL.getJoystickAxes js))
  pure $ JoystickSample <$> e

samplePointer ∷ ReflexGLFWCtx t m ⇒ GL.Window → Event t a → ReflexGLFW t m (Event t PointerSample)
samplePointer win ev = do
  e ← performEvent $ ev <&> (const $ liftIO $ GL.getCursorPos win)
  pure $ PointerSample <$> e

keyStateIsPress ∷ GL.KeyState → Bool
keyStateIsPress GL.KeyState'Pressed   = True
keyStateIsPress GL.KeyState'Repeating = True
keyStateIsPress _                     = False

mouseButtonStateIsPress ∷ GL.MouseButtonState → Bool
mouseButtonStateIsPress GL.MouseButtonState'Pressed = True
mouseButtonStateIsPress _                           = False

cursorPosCoords ∷ Input CursorPos → (Double, Double)
cursorPosCoords (EventCursorPos _ x y) = (x, y)

scrollX, scrollY ∷ Input Scroll → Double
scrollX (EventScroll _ x _) = x
scrollY (EventScroll _ _ y) = y


-- * Input Events
--
-- | The type describing deliverable GLFW events.
data EventType
  = Error
  | WindowPos
  | WindowSize
  | WindowClose
  | WindowRefresh
  | WindowFocus
  | WindowIconify
  | FramebufferSize
  | MouseButton
  | CursorPos
  | CursorEnter
  | Scroll
  | Key
  | Char
  deriving (Bounded, Enum, Eq, Ord, Show)

data InputU where
  U ∷ Input k → InputU
deriving instance Show InputU

data Input (k ∷ EventType) where
  EventError           ∷ GL.Error  → String                                                 → Input Error
  EventWindowPos       ∷ GL.Window → Int → Int                                              → Input WindowPos
  EventWindowSize      ∷ GL.Window → Int → Int                                              → Input WindowSize
  EventWindowClose     ∷ GL.Window                                                          → Input WindowClose
  EventWindowRefresh   ∷ GL.Window                                                          → Input WindowRefresh
  EventWindowFocus     ∷ GL.Window → Bool                                                   → Input WindowFocus
  EventWindowIconify   ∷ GL.Window → Bool                                                   → Input WindowIconify
  EventFramebufferSize ∷ GL.Window → Int → Int                                              → Input FramebufferSize
  EventMouseButton     ∷ GL.Window → GL.MouseButton → GL.MouseButtonState → GL.ModifierKeys → Input MouseButton
  EventCursorPos       ∷ GL.Window → Double → Double                                        → Input CursorPos
  EventCursorEnter     ∷ GL.Window → GL.CursorState                                         → Input CursorEnter
  EventScroll          ∷ GL.Window → Double → Double                                        → Input Scroll
  EventKey             ∷ GL.Window → GL.Key → Int → GL.KeyState → GL.ModifierKeys           → Input Key
  EventChar            ∷ GL.Window → Prelude.Char                                           → Input Char
deriving instance Show (Input k)

eventType ∷ Input u → EventType
eventType = \case
  EventError{}           → Error
  EventWindowPos{}       → WindowPos
  EventWindowSize{}      → WindowSize
  EventWindowClose{}     → WindowClose
  EventWindowRefresh{}   → WindowRefresh
  EventWindowFocus{}     → WindowFocus
  EventWindowIconify{}   → WindowIconify
  EventFramebufferSize{} → FramebufferSize
  EventMouseButton{}     → MouseButton
  EventCursorPos{}       → CursorPos
  EventCursorEnter{}     → CursorEnter
  EventScroll{}          → Scroll
  EventKey{}             → Key
  EventChar{}            → Char

eventUType ∷ InputU → EventType
eventUType (U i) = eventType i

data EventMask where
  EventMask ∷
    { emError           ∷ Bool
    , emWindowPos       ∷ Bool
    , emWindowSize      ∷ Bool
    , emWindowClose     ∷ Bool
    , emWindowRefresh   ∷ Bool
    , emWindowFocus     ∷ Bool
    , emWindowIconify   ∷ Bool
    , emFramebufferSize ∷ Bool
    , emMouseButton     ∷ Maybe ButtonEventMask
    , emCursorPos       ∷ Bool
    , emCursorEnter     ∷ Bool
    , emScroll          ∷ Bool
    , emKey             ∷ Maybe KeyEventMask
    , emChar            ∷ Bool
    } → EventMask
  deriving (Eq, Ord)
instance Show EventMask where
  show EventMask{..} = ("(EventMask"<>) ∘ (<>")") $ concat $
    [" Error"           | emError]           <>
    [" WindowPos"       | emWindowPos]       <>
    [" WindowSize"      | emWindowSize]      <>
    [" WindowClose"     | emWindowClose]     <>
    [" WindowRefresh"   | emWindowRefresh]   <>
    [" WindowFocus"     | emWindowFocus]     <>
    [" WindowIconify"   | emWindowIconify]   <>
    [" FramebufferSize" | emFramebufferSize] <>
    maybe [] ((:[]) ∘ (" "<>) ∘ show) emMouseButton <>
    [" CursorPos"       | emCursorPos]       <>
    [" CursorEnter"     | emCursorEnter]     <>
    maybe [] ((:[]) ∘ (" "<>) ∘ show) emKey  <>
    [" Char"            | emChar]

instance Semigroup EventMask where
  EventMask e wp ws wc we wf wi fs mb cp ce s k c <> EventMask e' wp' ws' wc' we' wf' wi' fs' mb' cp' ce' s' k' c' =
    EventMask (e∨e') (wp∨wp') (ws∨ws') (wc∨wc') (we∨we') (wf∨wf') (wi∨wi') (fs∨fs') (mb<>mb') (cp∨cp') (ce∨ce') (s∨s') (k<>k') (c∨c')
instance Monoid    EventMask where
  mempty = EventMask False False False False False False False False Nothing False False False Nothing False

eventMaskKeys       ∷ KeyEventMask    → EventMask
eventMaskKeys    ks = EventMask False False False False False False False False Nothing   False False False (Just ks) False

eventMaskChars      ∷ EventMask
eventMaskChars      = EventMask False False False False False False False False Nothing   False False False Nothing   True

eventMaskButtons    ∷ ButtonEventMask → EventMask
eventMaskButtons bs = EventMask False False False False False False False False (Just bs) False False False Nothing   False

eventTypeMaskTest ∷ EventMask → EventType → Bool
eventTypeMaskTest EventMask{..} = \case
  Error           → emError
  WindowPos       → emWindowPos
  WindowSize      → emWindowSize
  WindowClose     → emWindowClose
  WindowRefresh   → emWindowRefresh
  WindowFocus     → emWindowFocus
  WindowIconify   → emWindowIconify
  FramebufferSize → emFramebufferSize
  MouseButton     → isJust emMouseButton
  CursorPos       → emCursorPos
  CursorEnter     → emCursorEnter
  Scroll          → emScroll
  Key             → isJust emKey
  Char            → emChar

eventMaskTypes ∷ EventMask → [EventType]
eventMaskTypes = flip filter (enumFromTo minBound maxBound) ∘ eventTypeMaskTest

data ButtonEventMask where
  ButtonEventMask ∷
    { emButtons         ∷ Set GL.MouseButton
    , emButtonStates    ∷ Set GL.MouseButtonState
    , emButtonModifiers ∷ GL.ModifierKeys
    } → ButtonEventMask
  deriving (Eq, Ord)
instance Show ButtonEventMask where
  show (ButtonEventMask btns states mods) = ("(Mouse "<>) ∘ (<>")") $
    (intercalate "+" $ drop 5  ∘ show <$> Set.toList btns)   <> " " <>
    (intercalate "+" $ drop 18 ∘ show <$> Set.toList states) <> " " <>
    showModifierKeys mods

subsetModifierKeys ∷ GL.ModifierKeys → GL.ModifierKeys → Bool
subsetModifierKeys (GL.ModifierKeys a b c d) (GL.ModifierKeys a' b' c' d') =
  (not a ∨ a') ∧ (not b ∨ b') ∧ (not c ∨ c') ∧ (not d ∨ d')

showModifierKeys ∷ GL.ModifierKeys → String
showModifierKeys (GL.ModifierKeys shift ctrl alt super) = ("(Modif"<>) ∘ (<>")") $
    concat $ [" SHIFT" | shift] <> [" CTRL" | ctrl] <> [" ALT" | alt] <> [" SUPER" | super]

instance Semigroup ButtonEventMask where
  ButtonEventMask a c e <> ButtonEventMask b d f = ButtonEventMask (a <> b) (c <> d) (e <> f)
instance Monoid    ButtonEventMask where
  mempty = ButtonEventMask mempty mempty mempty

data KeyEventMask where
  KeyEventMask ∷
    { emKeys            ∷ Set GL.Key
    , emKeyStates       ∷ Set GL.KeyState
    , emKeyModifiers    ∷ GL.ModifierKeys
    } → KeyEventMask
  deriving (Eq, Ord)
instance Show KeyEventMask where
  show (KeyEventMask btns states mods) = ("(Key "<>) ∘ (<>")") $
    (intercalate "+" $ drop 4 ∘ show <$> Set.toList btns)   <> " " <>
    (intercalate "+" $ drop 9 ∘ show <$> Set.toList states) <> " " <>
    showModifierKeys mods

instance Semigroup KeyEventMask where
  KeyEventMask a c e <> KeyEventMask b d f = KeyEventMask (a <> b) (c <> d) (e <> f)
instance Monoid    KeyEventMask where
  mempty = KeyEventMask mempty mempty mempty

eventMatch ∷ EventMask → Input k → Bool
eventMatch EventMask{..} = \case
  EventCursorPos{}         → emCursorPos
  EventScroll{}            → emScroll
  EventChar{}              → emChar
  EventKey         _ key _ state mods
                           → case emKey of
                               Just KeyEventMask{..}    → key `Set.member` emKeys ∧ state `Set.member` emKeyStates ∧ (mods `subsetModifierKeys` emKeyModifiers)
                               Nothing → False
  EventMouseButton _ button state mods
                           → case emMouseButton of
                               Just ButtonEventMask{..} → button `Set.member` emButtons ∧ state `Set.member` emButtonStates ∧ (mods `subsetModifierKeys` emButtonModifiers)
                               Nothing → False
  EventWindowRefresh{}     → emWindowRefresh
  EventFramebufferSize{}   → emFramebufferSize
  EventWindowSize{}        → emWindowSize
  EventCursorEnter{}       → emCursorEnter
  EventWindowPos{}         → emWindowPos
  EventWindowClose{}       → emWindowClose
  EventWindowFocus{}       → emWindowFocus
  EventWindowIconify{}     → emWindowIconify
  EventError{}             → emError

filterError           ∷ Reflex t ⇒ Event t InputU → Event t (Input Error)
filterWindowRefresh   ∷ Reflex t ⇒ Event t InputU → Event t (Input WindowRefresh)
filterFramebufferSize ∷ Reflex t ⇒ Event t InputU → Event t (Input FramebufferSize)
filterScroll          ∷ Reflex t ⇒ Event t InputU → Event t (Input Scroll)
filterKey             ∷ Reflex t ⇒ Event t InputU → Event t (Input Key)
filterMouseButton     ∷ Reflex t ⇒ Event t InputU → Event t (Input MouseButton)
filterCursorPos       ∷ Reflex t ⇒ Event t InputU → Event t (Input CursorPos)
filterChar            ∷ Reflex t ⇒ Event t InputU → Event t (Input Char)
filterError           = fmapMaybe (\case U e@(EventError _ _)             → Just e; _ → Nothing)
filterWindowRefresh   = fmapMaybe (\case U e@(EventWindowRefresh _)       → Just e; _ → Nothing)
filterFramebufferSize = fmapMaybe (\case U e@(EventFramebufferSize _ _ _) → Just e; _ → Nothing)
filterScroll          = fmapMaybe (\case U e@(EventScroll _ _ _)          → Just e; _ → Nothing)
filterKey             = fmapMaybe (\case U e@(EventKey _ _ _ _  _)        → Just e; _ → Nothing)
filterMouseButton     = fmapMaybe (\case U e@(EventMouseButton _ _ _ _)   → Just e; _ → Nothing)
filterCursorPos       = fmapMaybe (\case U e@(EventCursorPos _ _ _)       → Just e; _ → Nothing)
filterChar            = fmapMaybe (\case U e@(EventChar _ _)              → Just e; _ → Nothing)

filterMouseButton'      ∷ Reflex t   ⇒ GL.MouseButton → Event t (Input MouseButton) → Event t (Input MouseButton)
filterMouseButton'           btn       = ffilter (\case EventMouseButton _ k _ _ → k ≡ btn)
filterMouseButtonStateChange ∷ Reflex t ⇒ GL.MouseButton → Bool → Event t (Input MouseButton) → Event t (Input MouseButton)
filterMouseButtonStateChange btn state = ffilter (\case EventMouseButton _ k st _ → k ≡ btn ∧ mouseButtonStateIsPress st ≡ state)

filterKey', filterKeyPress       ∷ Reflex t   ⇒ GL.Key         → Event t (Input Key)         → Event t (Input Key)
filterKey'     key = ffilter (\case EventKey _ k _ _  _ → k ≡ key)
filterKeyPress key = ffilter (\case EventKey _ k _ ks _ → k ≡ key ∧ keyStateIsPress ks)

errorCallback           ∷ STM.TQueue InputU → GL.Error → String                                                    → IO ()
windowPosCallback       ∷ STM.TQueue InputU → GL.Window → Int → Int                                                → IO ()
windowSizeCallback      ∷ STM.TQueue InputU → GL.Window → Int → Int                                                → IO ()
windowCloseCallback     ∷ STM.TQueue InputU → GL.Window                                                            → IO ()
windowRefreshCallback   ∷ STM.TQueue InputU → GL.Window                                                            → IO ()
windowFocusCallback     ∷ STM.TQueue InputU → GL.Window → Bool                                                     → IO ()
windowIconifyCallback   ∷ STM.TQueue InputU → GL.Window → Bool                                                     → IO ()
framebufferSizeCallback ∷ STM.TQueue InputU → GL.Window → Int → Int                                                → IO ()
mouseButtonCallback     ∷ STM.TQueue InputU → GL.Window → GL.MouseButton   → GL.MouseButtonState → GL.ModifierKeys → IO ()
cursorPosCallback       ∷ STM.TQueue InputU → GL.Window → Double → Double                                          → IO ()
cursorEnterCallback     ∷ STM.TQueue InputU → GL.Window → GL.CursorState                                           → IO ()
scrollCallback          ∷ STM.TQueue InputU → GL.Window → Double → Double                                          → IO ()
keyCallback             ∷ STM.TQueue InputU → GL.Window → GL.Key → Int → GL.KeyState → GL.ModifierKeys             → IO ()
charCallback            ∷ STM.TQueue InputU → GL.Window → Prelude.Char                                             → IO ()
errorCallback           tc     e s        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventError           e s
windowPosCallback       tc win x y        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowSize      win w h
windowCloseCallback     tc win            = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowClose     win
windowRefreshCallback   tc win            = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventCursorEnter     win ca
scrollCallback          tc win x y        = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventScroll          win x y
keyCallback             tc win k sc ka mk = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventKey             win k sc ka mk
charCallback            tc win c          = STM.atomically ∘ STM.writeTQueue tc ∘ U $ EventChar            win c

enableErrorEvents            ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowPosEvents        ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowSizeEvents       ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowCloseEvents      ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowRefreshEvents    ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowFocusEvents      ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableWindowIconifyEvents    ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableFramebufferSizeEvents  ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableMouseButtonEvents      ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableCursorPosEvents        ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableCursorEnterEvents      ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableScrollEvents           ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableKeyEvents              ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
enableCharEvents             ∷ MonadIO m ⇒ STM.TQueue InputU → GL.Window → m ()
disableErrorEvents           ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowPosEvents       ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowSizeEvents      ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowCloseEvents     ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowRefreshEvents   ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowFocusEvents     ∷ MonadIO m ⇒                     GL.Window → m ()
disableWindowIconifyEvents   ∷ MonadIO m ⇒                     GL.Window → m ()
disableFramebufferSizeEvents ∷ MonadIO m ⇒                     GL.Window → m ()
disableMouseButtonEvents     ∷ MonadIO m ⇒                     GL.Window → m ()
disableCursorPosEvents       ∷ MonadIO m ⇒                     GL.Window → m ()
disableCursorEnterEvents     ∷ MonadIO m ⇒                     GL.Window → m ()
disableScrollEvents          ∷ MonadIO m ⇒                     GL.Window → m ()
disableKeyEvents             ∷ MonadIO m ⇒                     GL.Window → m ()
disableCharEvents            ∷ MonadIO m ⇒                     GL.Window → m ()
enableErrorEvents            iq   _ = liftIO $ GL.setErrorCallback               $ Just $ errorCallback           iq
enableWindowPosEvents        iq win = liftIO $ GL.setWindowPosCallback       win $ Just $ windowPosCallback       iq
enableWindowSizeEvents       iq win = liftIO $ GL.setWindowSizeCallback      win $ Just $ windowSizeCallback      iq
enableWindowCloseEvents      iq win = liftIO $ GL.setWindowCloseCallback     win $ Just $ windowCloseCallback     iq
enableWindowRefreshEvents    iq win = liftIO $ GL.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   iq
enableWindowFocusEvents      iq win = liftIO $ GL.setWindowFocusCallback     win $ Just $ windowFocusCallback     iq
enableWindowIconifyEvents    iq win = liftIO $ GL.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   iq
enableFramebufferSizeEvents  iq win = liftIO $ GL.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback iq
enableMouseButtonEvents      iq win = liftIO $ GL.setMouseButtonCallback     win $ Just $ mouseButtonCallback     iq
enableCursorPosEvents        iq win = liftIO $ GL.setCursorPosCallback       win $ Just $ cursorPosCallback       iq
enableCursorEnterEvents      iq win = liftIO $ GL.setCursorEnterCallback     win $ Just $ cursorEnterCallback     iq
enableScrollEvents           iq win = liftIO $ GL.setScrollCallback          win $ Just $ scrollCallback          iq
enableKeyEvents              iq win = liftIO $ GL.setKeyCallback             win $ Just $ keyCallback             iq
enableCharEvents             iq win = liftIO $ GL.setCharCallback            win $ Just $ charCallback            iq
disableErrorEvents                _ = liftIO $ GL.setErrorCallback               $ Nothing
disableWindowPosEvents          win = liftIO $ GL.setWindowPosCallback       win $ Nothing
disableWindowSizeEvents         win = liftIO $ GL.setWindowSizeCallback      win $ Nothing
disableWindowCloseEvents        win = liftIO $ GL.setWindowCloseCallback     win $ Nothing
disableWindowRefreshEvents      win = liftIO $ GL.setWindowRefreshCallback   win $ Nothing
disableWindowFocusEvents        win = liftIO $ GL.setWindowFocusCallback     win $ Nothing
disableWindowIconifyEvents      win = liftIO $ GL.setWindowIconifyCallback   win $ Nothing
disableFramebufferSizeEvents    win = liftIO $ GL.setFramebufferSizeCallback win $ Nothing
disableMouseButtonEvents        win = liftIO $ GL.setMouseButtonCallback     win $ Nothing
disableCursorPosEvents          win = liftIO $ GL.setCursorPosCallback       win $ Nothing
disableCursorEnterEvents        win = liftIO $ GL.setCursorEnterCallback     win $ Nothing
disableScrollEvents             win = liftIO $ GL.setScrollCallback          win $ Nothing
disableKeyEvents                win = liftIO $ GL.setKeyCallback             win $ Nothing
disableCharEvents               win = liftIO $ GL.setCharCallback            win $ Nothing


-- * Safe input queue manipulation
--
newtype EventCtl = EventCtl (GL.Window, STM.TQueue InputU)

makeEventCtl ∷ (MonadIO m) ⇒ GL.Window → m EventCtl
makeEventCtl win = liftIO $ do
  iq ← STM.newTQueueIO
  let ec = EventCtl (win, iq)
  forM_ defaultEnabledEvents $ \et→
    enableEvent ec et
  pure ec

readInput ∷ (MonadIO m) ⇒ EventCtl → m (Maybe InputU)
readInput (EventCtl (_, queue)) = liftIO $ do
  GL.pollEvents
  STM.atomically $ STM.tryReadTQueue queue

defaultEnabledEvents ∷ [EventType]
defaultEnabledEvents =
  [Error, MouseButton, CursorPos, Scroll, Key, Char]

eventControlMap ∷ MonadIO m ⇒ Map EventType
                                  (STM.TQueue InputU → GL.Window → m ()
                                  ,                    GL.Window → m ())
eventControlMap = Map.fromList
  [(Error,           (enableErrorEvents,           disableErrorEvents))
  ,(WindowPos,       (enableWindowPosEvents,       disableWindowPosEvents))
  ,(WindowSize,      (enableWindowSizeEvents,      disableWindowSizeEvents))
  ,(WindowClose,     (enableWindowCloseEvents,     disableWindowCloseEvents))
  ,(WindowRefresh,   (enableWindowRefreshEvents,   disableWindowRefreshEvents))
  ,(WindowFocus,     (enableWindowFocusEvents,     disableWindowFocusEvents))
  ,(WindowIconify,   (enableWindowIconifyEvents,   disableWindowIconifyEvents))
  ,(FramebufferSize, (enableFramebufferSizeEvents, disableFramebufferSizeEvents))
  ,(MouseButton,     (enableMouseButtonEvents,     disableMouseButtonEvents))
  ,(CursorPos,       (enableCursorPosEvents,       disableCursorPosEvents))
  ,(CursorEnter,     (enableCursorEnterEvents,     disableCursorEnterEvents))
  ,(Scroll,          (enableScrollEvents,          disableScrollEvents))
  ,(Key,             (enableKeyEvents,             disableKeyEvents))
  ,(Char,            (enableCharEvents,            disableCharEvents))]

-- | Control firing of events of 'EventType'.
setEvent ∷ Bool → EventCtl → EventType → IO ()
setEvent True  (EventCtl (win, iq)) evType =
  (fst ∘ fromJust $ Map.lookup evType eventControlMap) iq win
setEvent False (EventCtl (win,  _)) evType =
  (snd ∘ fromJust $ Map.lookup evType eventControlMap)    win

-- | Specialised versions of 'setEvent'.
enableEvent, disableEvent ∷ EventCtl → EventType → IO ()
enableEvent  = setEvent True
disableEvent = setEvent False


-- * Input Dynamics
--
mouseButtonState ∷ ReflexGLFWCtx t m ⇒ GL.MouseButton → Event t (Input MouseButton) → ReflexGLFW t m (Dynamic t (Maybe (Double, Double)))
mouseButtonState btn inputE = do
  -- XXX/optimise:  maybe 'toggle' is to help us here?
  e ← performEvent $ filterMouseButton' btn inputE <&>
    (\case EventMouseButton win _ ks _ → do
             if mouseButtonStateIsPress ks
             then Just <$> (liftIO $ GL.getCursorPos win)
             else pure Nothing)
  holdDyn Nothing e

keyState ∷ ReflexGLFWCtx t m ⇒ GL.Key → Event t (Input Key) → ReflexGLFW t m (Dynamic t Bool)
keyState key inputE =
  -- XXX/optimise:  maybe 'toggle' is to help us here?
  holdDyn False $ (\case EventKey _ _ _ ks _       → keyStateIsPress ks)         <$> filterKey' key inputE


-- * Reflex hosts
--
-- | A Reflex host that sets up a default GL window, executes
--   'defaultGLWindowSetup' on it and yields to the guest.  Performs no extra
--   cleanup at the end.  Like 'withGLWindow', depends on 'init' / 'tryInit' to be
--   performed beforehand.
simpleHost ∷ (MonadIO io)
           ⇒ String              -- ^ GL window title
           → (∀ t m. ReflexGLFWGuest t m)
           → io ()
simpleHost title guest =
  withGLWindow 1024 768 title $ \win → do
    defaultGLWindowSetup win
    host win guest

-- | Like 'simpleHost', but also performs 'init' itself.  Note, that this limits
--   the GL profile to whatever is provided by 'GLFW.defaultWindowHints'.
basicHost ∷ (MonadIO io) ⇒ String → (∀ t m. ReflexGLFWGuest t m) → io ()
basicHost title guest = init >> simpleHost title guest

-- | Like 'basicHost', but requests a forward-compatible OpenGL 3.3 Core profile.
basicGL33Host ∷ (MonadIO io) ⇒ String → (∀ t m. ReflexGLFWGuest t m) → io ()
basicGL33Host title guest = init >> gl33ForwardCoreSetup >> simpleHost title guest

-- | A Reflex host runs a program written in the framework.  This will do all the
--   necessary work to integrate the Reflex-based guest program with the outside
--   world via IO.
host ∷ (MonadIO io)
     ⇒ GL.Window                     -- ^ A GL window to operate on.  One could be
                                     --   made by 'withGLWindow'.
     → (∀ t m. ReflexGLFWGuest t m)  -- ^ The user FRP network, aka "guest"
     → io ()
host win guest = do
  ec ← makeEventCtl win
  events ← liftIO newChan

  fireRef ← liftIO $ newRef Nothing

  -- 1. A forked EventTrigger forwarder
  void . liftIO . forkIO $ do
    -- Wait for the FireCommand to appear, first:
    let fireWaitLoop = do
          mFire ← readRef fireRef
          case mFire of
            Nothing   → threadDelay 100000 >> fireWaitLoop
            Just fire → pure fire
    FireCommand fire ← fireWaitLoop
    -- Actual event forwarding:
    forever $ do
      ers <- readChan events
      _ <- runSpiderHost $ do
        mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
          me <- readIORef er
          return $ fmap (\e -> e :=> Identity a) me
        _ <- fire (catMaybes mes) $ return ()
        liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
      pure ()
    pure ()

  liftIO $ runSpiderHost $ do
    -- 2. Build the FRP network and obtain:
    --    - (non-EventTrigger-ed) event injectors
    --    - the termination token
    ((b, postBuildTriggerRef, frameriggerRef, inputTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      (frame,     frameriggerRef)      <- newEventWithTriggerRef
      (input,     inputTriggerRef)     <- newEventWithTriggerRef
      b <- runTriggerEventT (runPostBuildT (guest win ec frame input) postBuild) events
      pure ( b
           , postBuildTriggerRef, frameriggerRef, inputTriggerRef)
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()

    -- 3. Start up the EventTrigger event forwarder:
    writeRef fireRef $ Just fc

    -- 4. Keep feeding frame and input events:
    let loop ∷ SpiderHost Global ()
        loop = do
          (stopRequest, FireCommand _) ← hostPerformEventT $ sample b

          mTrig' ← liftIO $ readIORef frameriggerRef
          case mTrig' of
            Nothing   → pure [()]
            Just trig →
              fire [trig :=> Identity win] $ pure ()

          let inputInnerLoop = do
                mInput ← readInput ec
                case mInput of
                  Nothing → pure ()
                  Just input → do
                    mTrig'' ← liftIO $ readIORef inputTriggerRef
                    case mTrig'' of
                      Nothing → pure [()]
                      Just trig → fire [trig :=> Identity input] $ pure ()
                    inputInnerLoop
          inputInnerLoop

          unless stopRequest $
            loop
    loop

  pure ()
