{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | GLFW windows and Vulkan presentation surfaces for vpipe.

GLFW requires window creation, event processing, and termination to happen on
the main OS thread on macOS (and is easiest to use that way everywhere).  Bind
the application entry point to an OS thread:

@
import Control.Concurrent (runInBoundThread)
import Control.Monad (unless)
import Vpipe.Context (defaultVpipeConfig)
import Vpipe.GLFW

main :: IO ()
main =
  runInBoundThread $
    withWindow defaultVpipeConfig defaultWindowConfig $ \_ window ->
      let eventLoop = do
            pollEvents
            escape <- (== KeyPressed) <$> getKey window KeyEscape
            unless escape eventLoop
       in eventLoop
@

Rendering work may be moved to other threads, but calls in this module that
touch GLFW should remain on the thread which created the windows.
-}
module Vpipe.GLFW (
  Key,
  pattern KeyEscape,
  pattern KeySpace,
  pattern KeyEnter,
  pattern KeyLeft,
  pattern KeyRight,
  pattern KeyUp,
  pattern KeyDown,
  pattern KeyW,
  pattern KeyA,
  pattern KeyS,
  pattern KeyD,
  KeyState,
  pattern KeyPressed,
  pattern KeyReleased,
  pattern KeyRepeating,
  Window,
  WindowConfig (..),
  defaultWindowConfig,
  withWindow,
  withWindows,
  windowSurface,
  requiredInstanceExtensions,
  pollEvents,
  waitEvents,
  windowShouldClose,
  requestWindowClose,
  resizeWindow,
  getFramebufferSize,
  getKey,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, mask, onException, throwIO, try)
import Control.Monad (unless, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Either (lefts)
import Data.Foldable (traverse_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import Graphics.UI.GLFW qualified as GLFW
import Vulkan.Core10.Enums.Result qualified as Result
import Vulkan.Core10.Handles qualified as Handles
import Vulkan.Extensions.Handles qualified as Handles
import Vulkan.Extensions.VK_KHR_surface qualified as Surface

import Vpipe.Context (Context, VpipeConfig)
import Vpipe.Error (VpipeError (..))
import Vpipe.Surface (Surface, withVpipeSurfaces)
import Vpipe.Surface.Driver (mkSurfaceFactoryWithExtents)

{- | A keyboard key accepted by 'getKey'.

The common application keys are available as the documented patterns in
this module.  The alias preserves GLFW-b's key representation, so an
application can also pass an integration-specific key without conversion.
-}
type Key = GLFW.Key

-- | The Escape key.
pattern KeyEscape :: Key
pattern KeyEscape = GLFW.Key'Escape

-- | The Space key.
pattern KeySpace :: Key
pattern KeySpace = GLFW.Key'Space

-- | The Enter key.
pattern KeyEnter :: Key
pattern KeyEnter = GLFW.Key'Enter

-- | The Left Arrow key.
pattern KeyLeft :: Key
pattern KeyLeft = GLFW.Key'Left

-- | The Right Arrow key.
pattern KeyRight :: Key
pattern KeyRight = GLFW.Key'Right

-- | The Up Arrow key.
pattern KeyUp :: Key
pattern KeyUp = GLFW.Key'Up

-- | The Down Arrow key.
pattern KeyDown :: Key
pattern KeyDown = GLFW.Key'Down

-- | The W key.
pattern KeyW :: Key
pattern KeyW = GLFW.Key'W

-- | The A key.
pattern KeyA :: Key
pattern KeyA = GLFW.Key'A

-- | The S key.
pattern KeyS :: Key
pattern KeyS = GLFW.Key'S

-- | The D key.
pattern KeyD :: Key
pattern KeyD = GLFW.Key'D

-- | The state returned by 'getKey'.
type KeyState = GLFW.KeyState

-- | The key is currently pressed.
pattern KeyPressed :: KeyState
pattern KeyPressed = GLFW.KeyState'Pressed

-- | The key is not pressed.
pattern KeyReleased :: KeyState
pattern KeyReleased = GLFW.KeyState'Released

-- | The key is pressed and GLFW is generating repeat events.
pattern KeyRepeating :: KeyState
pattern KeyRepeating = GLFW.KeyState'Repeating

{- | A GLFW native window paired with the vpipe surface created for it.
Constructors are hidden so a window cannot outlive its context.
-}
data Window = Window
  { nativeWindow :: GLFW.Window
  , windowSurface :: Surface
  -- ^ The presentation surface associated with this window.
  }

data WindowResourceOwner
  = WindowScope
  | SurfacePayload
  deriving stock (Eq, Show)

data WindowResourceOwnership
  = WindowsOwnedBy WindowResourceOwner
  | WindowResourcesReleased
  deriving stock (Eq, Show)

data WindowPayload = WindowPayload (NonEmpty GLFW.Window) (MVar WindowResourceOwnership)

-- | Properties used to create a windowed Vulkan surface.
data WindowConfig = WindowConfig
  { windowWidth :: Int
  , windowHeight :: Int
  , windowTitle :: String
  , windowResizable :: Bool
  }
  deriving stock (Eq, Show)

-- | A resizable 1280×720 window titled @vpipe@.
defaultWindowConfig :: WindowConfig
defaultWindowConfig =
  WindowConfig
    { windowWidth = 1280
    , windowHeight = 720
    , windowTitle = "vpipe"
    , windowResizable = True
    }

-- | Acquire one GLFW window and a context which can present to it.
withWindow :: VpipeConfig -> WindowConfig -> (Context -> Window -> IO a) -> IO a
withWindow config windowConfig action =
  withWindows config (pure windowConfig) $ \context windows -> action context (NonEmpty.head windows)

{- | Acquire GLFW windows and a context which can present to all of them.

GLFW is initialized and terminated by this function on the invoking OS
thread.  Windows are destroyed only after vpipe has released the Vulkan
resources and surfaces which reference them.
-}
withWindows :: VpipeConfig -> NonEmpty WindowConfig -> (Context -> NonEmpty Window -> IO a) -> IO a
withWindows config configs action = mask $ \restore -> do
  initialized <- GLFW.init
  unless initialized (GLFW.terminate >> throwIO (VulkanFailure "glfwInit" "initialization failed"))
  extensions <-
    ( do
        supported <- GLFW.vulkanSupported
        unless supported (throwIO (NoVulkanIcd "GLFW could not find the Vulkan loader"))
        copyRequiredInstanceExtensions
    )
      `onException` GLFW.terminate
  nativeWindows <- createNativeWindows configs `onException` GLFW.terminate
  ownership <- newMVar (WindowsOwnedBy WindowScope)
  let acquireWindowSurfaces instance' = do
        createdSurfaces <- createSurfaces nativeWindows instance'
        let destroyCreatedSurfaces =
              traverse_ (\(surface, _) -> Surface.destroySurfaceKHR instance' surface Nothing) createdSurfaces
        transferWindowResourceOwnership ownership `onException` destroyCreatedSurfaces
        pure (WindowPayload nativeWindows ownership, createdSurfaces)
      releaseWindowPayload (WindowPayload windows payloadOwnership) =
        releaseWindowResources SurfacePayload payloadOwnership windows
      factory =
        mkSurfaceFactoryWithExtents
          extensions
          acquireWindowSurfaces
          releaseWindowPayload
      runWithSurfaces =
        withVpipeSurfaces config factory $ \context surfaces (WindowPayload windows _) ->
          action context (NonEmpty.zipWith Window windows surfaces)
  restore runWithSurfaces
    `onException` releaseWindowResources WindowScope ownership nativeWindows

{- | Return GLFW's required Vulkan instance extensions as copied bytes.

GLFW must already be initialized. @withWindow@ and @withWindows@ do this
internally; callers using this function to construct a surface factory must
arrange their own GLFW initialization and retain it until instance creation.
-}
requiredInstanceExtensions :: IO [ByteString]
requiredInstanceExtensions = copyRequiredInstanceExtensions

-- | Process pending GLFW events.
pollEvents :: IO ()
pollEvents = GLFW.pollEvents

-- | Block until and then process a GLFW event.
waitEvents :: IO ()
waitEvents = GLFW.waitEvents

-- | Whether the user or application requested that this window close.
windowShouldClose :: Window -> IO Bool
windowShouldClose = GLFW.windowShouldClose . nativeWindow

-- | Request that the application close this window at its next event-loop check.
requestWindowClose :: Window -> IO ()
requestWindowClose window = GLFW.setWindowShouldClose (nativeWindow window) True

{- | Resize a window's client area in screen coordinates.

The framebuffer may have a different extent on high-density displays; query
'getFramebufferSize' before creating size-dependent rendering resources.
-}
resizeWindow :: Window -> Int -> Int -> IO ()
resizeWindow window width height
  | width <= 0 || height <= 0 =
      throwIO
        ( VulkanFailure
            "glfwSetWindowSize"
            ("expected positive dimensions, received " <> show (width, height))
        )
  | otherwise = GLFW.setWindowSize (nativeWindow window) width height

{- | The current framebuffer extent in physical pixels.

This calls GLFW directly and must run on the thread which created the window.
Swapchain rendering uses an internal callback-backed cache so frames may run
on worker threads.
-}
getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize = GLFW.getFramebufferSize . nativeWindow

-- | Query the current state of a GLFW key.
getKey :: Window -> Key -> IO KeyState
getKey window = GLFW.getKey (nativeWindow window)

copyRequiredInstanceExtensions :: IO [ByteString]
copyRequiredInstanceExtensions = traverse copyCString =<< GLFW.getRequiredInstanceExtensions
 where
  copyCString :: CString -> IO ByteString
  copyCString pointer = ByteString.Char8.pack <$> peekCString pointer

transferWindowResourceOwnership :: MVar WindowResourceOwnership -> IO ()
transferWindowResourceOwnership ownership =
  modifyMVar ownership $ \case
    WindowsOwnedBy WindowScope -> pure (WindowsOwnedBy SurfacePayload, ())
    WindowsOwnedBy SurfacePayload ->
      throwIO (CleanupFailed ["GLFW window resources were already transferred to the surface payload"])
    WindowResourcesReleased ->
      throwIO (CleanupFailed ["GLFW window resources were released before transfer to the surface payload"])

releaseWindowResources :: WindowResourceOwner -> MVar WindowResourceOwnership -> NonEmpty GLFW.Window -> IO ()
releaseWindowResources expectedOwner ownership windows = do
  shouldRelease <-
    modifyMVar ownership $ \current -> case current of
      WindowsOwnedBy actualOwner
        | actualOwner == expectedOwner -> pure (WindowResourcesReleased, True)
      WindowsOwnedBy WindowScope
        | expectedOwner == SurfacePayload ->
            throwIO (CleanupFailed ["the GLFW surface payload tried to release window resources before receiving ownership"])
      _ -> pure (current, False)
  when shouldRelease $ do
    windowResults <- traverse (\window -> try (GLFW.destroyWindow window) :: IO (Either SomeException ())) windows
    terminationResult <- try GLFW.terminate
    case lefts (NonEmpty.toList windowResults <> [terminationResult]) of
      firstError : _ -> throwIO firstError
      [] -> pure ()

createNativeWindows :: NonEmpty WindowConfig -> IO (NonEmpty GLFW.Window)
createNativeWindows configs = do
  created <- newIORef []
  let create config = do
        GLFW.defaultWindowHints
        GLFW.windowHint (GLFW.WindowHint'ClientAPI GLFW.ClientAPI'NoAPI)
        GLFW.windowHint (GLFW.WindowHint'Resizable (windowResizable config))
        maybeWindow <- GLFW.createWindow (windowWidth config) (windowHeight config) (windowTitle config) Nothing Nothing
        window <- maybe (throwIO (VulkanFailure "glfwCreateWindow" "window creation failed")) pure maybeWindow
        modifyIORef' created (window :)
        pure window
      destroyCreated = readIORef created >>= traverse_ GLFW.destroyWindow
  traverse create configs `onException` destroyCreated

createSurfaces :: NonEmpty GLFW.Window -> Handles.Instance -> IO (NonEmpty (Handles.SurfaceKHR, IO (Int, Int)))
createSurfaces windows instance' = do
  created <- newIORef []
  let create window = do
        initialExtent <- GLFW.getFramebufferSize window
        cachedExtent <- newMVar initialExtent
        GLFW.setFramebufferSizeCallback window (Just (\_ width height -> modifyMVar_ cachedExtent (const (pure (width, height)))))
        surface <- createSurface instance' window
        modifyIORef' created (surface :)
        pure (surface, readMVar cachedExtent)
      destroyCreated = readIORef created >>= traverse_ (\surface -> Surface.destroySurfaceKHR instance' surface Nothing)
  traverse create windows `onException` destroyCreated

createSurface :: Handles.Instance -> GLFW.Window -> IO Handles.SurfaceKHR
createSurface instance' window = alloca $ \surfacePointer -> do
  result <- GLFW.createWindowSurface (Handles.instanceHandle instance') window nullPtr surfacePointer :: IO GLFWResult
  vulkanResult <- toVulkanResult result
  case vulkanResult of
    Result.SUCCESS -> peek surfacePointer
    Result.ERROR_SURFACE_LOST_KHR -> throwIO SurfaceLost
    Result.ERROR_DEVICE_LOST -> throwIO DeviceLost
    failure -> throwIO (VulkanFailure "glfwCreateWindowSurface" (show failure))

-- GLFW-b exposes @VkResult@ through an @Enum@ constraint, while vulkan's
-- extensible Result newtype deliberately has no Enum instance.  This wrapper
-- preserves GLFW-b's raw integer ABI without inventing an invalid Result enum.
newtype GLFWResult = GLFWResult CInt

instance Enum GLFWResult where
  toEnum = GLFWResult . fromIntegral
  fromEnum (GLFWResult result) = fromIntegral result

toVulkanResult :: GLFWResult -> IO Result.Result
toVulkanResult (GLFWResult result) =
  alloca $ \(pointer :: Ptr CInt) -> do
    poke pointer result
    peek (castPtr pointer)
