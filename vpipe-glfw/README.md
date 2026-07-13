# vpipe-glfw

`vpipe-glfw` is the optional window-system package for
[`vpipe`](https://github.com/mewhhaha/vpipe). It creates GLFW windows and the
Vulkan context, surfaces, presentation queues, and swapchains that must be
negotiated with them.

Keeping this integration separate means headless `vpipe` applications do not
link GLFW.

## Use it

```cabal
build-depends:
  vpipe >=0.1 && <0.2,
  vpipe-glfw >=0.1 && <0.2
```

Window creation and event processing should remain on a bound OS thread:

```haskell
import Control.Concurrent (runInBoundThread)
import Vpipe.Context (defaultVpipeConfig)
import Vpipe.GLFW

main :: IO ()
main =
  runInBoundThread $
    withWindow defaultVpipeConfig defaultWindowConfig $ \context window -> do
      -- Allocate resources with context, render through windowSurface window,
      -- and call pollEvents from the window loop.
      pure ()
```

`withWindow` brackets GLFW initialization, Vulkan context and surface creation,
and cleanup in the required order. The module also provides multi-window
scopes, framebuffer-size queries, resize requests, event polling, close
requests, and common keyboard-key patterns.

See the complete
[first-triangle tutorial](https://github.com/mewhhaha/vpipe/blob/main/vpipe/docs/tutorials/first-triangle.md)
and the maintained
[windowed examples](https://github.com/mewhhaha/vpipe/tree/main/examples).

## Requirements

`vpipe-glfw` targets desktop Vulkan 1.3 and supports GHC 9.12.4 and 9.14.1. It
needs the Vulkan loader and headers, GLFW, and a Vulkan 1.3 driver. It is MIT
licensed.
