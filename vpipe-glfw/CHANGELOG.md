# Changelog

All notable changes to `vpipe-glfw` are recorded here. The project follows the
Haskell Package Versioning Policy.

## 0.1.0.0 — unreleased

- Added bracketed single- and multi-window GLFW scopes that negotiate required
  Vulkan instance extensions before context creation.
- Added managed Vulkan surface creation, presentation-family selection,
  framebuffer extent queries, event polling, key queries, and documented
  main-thread usage without adding GLFW to the headless core package.
