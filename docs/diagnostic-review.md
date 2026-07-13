# Diagnostics review

This is a doc-first review of the beginner failure paths named in [Task
14](../tasks/14-diagnostics.md). It is a source-and-fixture review, not a
claim of external issue reports or user testing. Each item remains locally
tracked here through its regression coverage and implementation link.

| Local item | Naive mistake | Current diagnostic and fix | Regression fixture | Resolution status |
| --- | --- | --- | --- | --- |
| D14-01 | Bind a buffer as a uniform without `Uniform` usage. | `Buffer operation requires usage Uniform, but this buffer's usage list does not contain it.` Add `Uniform` to the buffer usage list. | [`MissingUniformUsage`](../vpipe/test/type-errors/pipeline/MissingUniformUsage.hs) · [`HasUsage`](../vpipe/src/Vpipe/Buffer/Internal.hs) | Resolved; the compile-fail fixture keeps the named usage diagnostic. |
| D14-02 | Pass a fragment-stage position to `rasterize`. | `rasterize` says that it needs a vertex-stage clip position and directs the user to build `V (V4 Float)` with `vertexInput`. | [`WrongStage`](../vpipe/test/type-errors/pipeline/WrongStage.hs) · [`VertexStagePosition`](../vpipe/src/Vpipe/Pipeline/Internal.hs) | Resolved; the fixture checks both the stage error and remedy. |
| D14-03 | Send a scalar color to an RGBA target, or pass a depth target to `drawColor`. | `drawColor` names the target format, expected color value, and actual color value, then recommends `vec2`, `vec3`, or `vec4` for vector formats; depth targets direct the user to `drawDepth`. | [`OutputMismatch`](../vpipe/test/type-errors/pipeline/OutputMismatch.hs) · [`OutputVectorWidthMismatch`](../vpipe/test/type-errors/pipeline/OutputVectorWidthMismatch.hs) · [`DrawColorDepthTarget`](../vpipe/test/type-errors/pipeline/DrawColorDepthTarget.hs) · [`ColorOutputMatches`](../vpipe/src/Vpipe/Pipeline/Internal.hs) | Resolved; a closed diagnostic constraint checks the target format against the output type without overlapping instances. |
| D14-04 | Create a GLFW presentation context without GLFW's required instance extensions. | The exception lists missing extensions and says to use `Vpipe.GLFW.withWindow`, or to pass `Vpipe.GLFW.requiredInstanceExtensions` before creating a custom context. | [`ErrorTest`](../vpipe/test/Vpipe/ErrorTest.hs) · [`RequiredInstanceExtensionsUnavailable`](../vpipe/src/Vpipe/Error.hs) | Resolved; the runtime test checks both GLFW remedies. |
| D14-05 | Run without an installable Vulkan ICD. | `NoVulkanIcd` says to install a Vulkan ICD and names Mesa lavapipe as the headless Linux option. | [`ErrorTest`](../vpipe/test/Vpipe/ErrorTest.hs) · [`NoVulkanIcd`](../vpipe/src/Vpipe/Error.hs) | Resolved; the runtime test checks the install and lavapipe guidance. |
| D14-06 | Combine vertex streams whose primitive topologies differ. | `zipStreams` names its expected and actual topologies and says to give both sources the same `PrimitiveTopology`. | [`WrongTopology`](../vpipe/test/type-errors/pipeline/WrongTopology.hs) · [`MatchingTopology`](../vpipe/src/Vpipe/Pipeline/Internal.hs) | Resolved; the fixture checks the operation, both topologies, and the remedy. |

The [first-triangle troubleshooting section](../vpipe/docs/tutorials/first-triangle.md#if-it-fails)
is the user-facing route to the two runtime remedies. The type-error fixtures
are compiled by [`Vpipe.TypeErrorTest`](../vpipe/test/Vpipe/TypeErrorTest.hs),
which checks every `-- EXPECT:` substring. The harness deliberately uses a
fresh `ghc -fno-code` subprocess instead of `-fdefer-type-errors`: every
fixture must fail compilation, so an error cannot disappear merely because a
deferred binding was not evaluated.
