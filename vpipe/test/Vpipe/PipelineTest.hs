{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Vpipe.PipelineTest (pipelineTests) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (MaskingState (MaskedInterruptible), getMaskingState)
import Data.Bifunctor (bimap)
import Data.Bits ((.|.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Int (Int32)
import Data.Unique (newUnique)
import Foreign.Storable (peekByteOff)
import GHC.Generics (Generic)
import Linear (M44, V2 (..), V3 (..), V4 (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Vpipe.Buffer.Format (FieldLayout (..), MatrixBuffer, ScalarType (..), toMatrixBuffer)
import Vpipe.Buffer.State qualified as BufferState
import Vpipe.Expr
import Vpipe.Expr.Internal (HostValue (..), ImageDimension (..), ShaderTy (..))
import Vpipe.Expr.Reify (NodeId, ReifiedForest (..), ReifiedNode (..), ReifiedOp (..))
import Vpipe.Format (Blendable, ColorRenderable, Format (..), KnownFormat)
import Vpipe.Image.Types (Dim (D3))
import Vpipe.Pipeline.Internal
import Vpipe.Pipeline.Resource.Internal qualified as Resource
import Vpipe.Resource.Lifetime qualified as Lifetime
import Vulkan.Core10.Enums.BufferUsageFlagBits qualified as BufferUsage
import Vulkan.Core10.Enums.Format qualified as Vk
import Vulkan.Core10.Handles qualified as Handles

pipelineTests :: TestTree
pipelineTests =
  testGroup
    "pipeline"
    [ testCase "retains nonconstant M2 triangle varyings on both stage interfaces" typedTriangleCase
    , goldenVsString "records a representative pipeline interface" "test/golden/pipeline/representative-interface.table" representativeInterfaceGolden
    , testCase "decomposes tuple inputs and topology-safely zips attribute streams" tupleAndZipCase
    , testCase "assigns raw integer varyings flat interpolation" integerFlatCase
    , testCase "records color, depth override, and discard roots by role" structuredOutputsCase
    , testCase "retains the topology and target of each mixed draw" multipleDrawsCase
    , testCase "assigns color locations independently for each draw" colorLocationsCase
    , testCase "accepts scalar and generic color outputs" colorOutputCase
    , testCase "retains discard predicates regardless of output declaration order" discardOrderCase
    , testCase "decomposes GenericVertex record fields" genericVertexCase
    , testCase "assigns two uniforms and one texture stable bindings" resourceBindingsCase
    , testCase "records sampled texture dimensions" sampledTextureDimensionCase
    , testCase "resolves concrete runtime resources through environment accessors" accessorResolutionCase
    , testCase "records storage and rejects uniform-storage handle aliases" storageResolutionCase
    , testCase "buffer alias fallback distinguishes managed owners across reused raw handles" managedBufferAliasOwnershipCase
    , testCase "runtime handle quarantine is total and masks managed callbacks" runtimeHandleQuarantineCase
    , testCase "records and resolves deterministic push-constant ranges" pushConstantCase
    , testCase "rejects a combined push-constant range above 128 bytes" pushConstantTotalLimitCase
    , testCase "rejects Boolean32 uniform and push-constant metadata" booleanBufferMetadataCase
    , testCase "rejects conflicting resource identities" resourceCollisionCase
    , testCase "rejects conflicting vertex source formats" vertexCollisionCase
    , testCase "rejects conflicting attachment formats" attachmentCollisionCase
    , testCase "rejects compatible-name accessors that resolve differently" accessorCollisionCase
    , testCase "records and resolves indexed draws outside the shader interface" indexedDrawCase
    ]

data TestEnvironment = TestEnvironment
  { environmentMesh :: VertexBuffer (V3 Float, V4 Float)
  , environmentPositions :: VertexBuffer (V3 Float)
  , environmentCoordinates :: VertexBuffer (V2 Float)
  , environmentMvp :: UniformBuffer (MatrixBuffer 4 4 Float)
  , environmentExposure :: UniformBuffer Float
  , environmentIdentifier :: UniformBuffer Int32
  , environmentStorage :: StorageBuffer Float
  , environmentPushScalar :: Float
  , environmentPushVector :: V4 Float
  , environmentTexture :: TextureBinding
  , environmentColor :: ColorImage 'R8G8B8A8Srgb
  , environmentLinearColor :: ColorImage 'R32Sfloat
  , environmentAlternateColor :: ColorImage 'R8G8B8A8Srgb
  , environmentDepth :: DepthImage 'D32Sfloat
  }

testEnvironment :: TestEnvironment
testEnvironment =
  TestEnvironment
    { environmentMesh = VertexBuffer (RuntimeHandle 10)
    , environmentPositions = VertexBuffer (RuntimeHandle 11)
    , environmentCoordinates = VertexBuffer (RuntimeHandle 12)
    , environmentMvp = UniformBuffer (RuntimeHandle 20)
    , environmentExposure = UniformBuffer (RuntimeHandle 21)
    , environmentIdentifier = UniformBuffer (RuntimeHandle 22)
    , environmentStorage = StorageBuffer (RuntimeHandle 23)
    , environmentPushScalar = 2.5
    , environmentPushVector = V4 1 2 3 4
    , environmentTexture = TextureBinding (RuntimeHandle 30) (RuntimeHandle 31)
    , environmentColor = ColorImage (RuntimeHandle 40)
    , environmentLinearColor = ColorImage (RuntimeHandle 41)
    , environmentAlternateColor = ColorImage (RuntimeHandle 42)
    , environmentDepth = DepthImage (RuntimeHandle 50)
    }

meshSource :: VertexSource TestEnvironment 'Triangles (V3 Float, V4 Float)
meshSource = vertexSource "mesh" environmentMesh

positionSource :: VertexSource TestEnvironment topology (V3 Float)
positionSource = vertexSource "position" environmentPositions

coordinateSource :: VertexSource TestEnvironment topology (V2 Float)
coordinateSource = vertexSource "coordinates" environmentCoordinates

mvpSource :: Uniform TestEnvironment (M44 Float)
mvpSource = uniformSource "mvp" environmentMvp

exposureSource :: Uniform TestEnvironment Float
exposureSource = uniformSource "exposure" environmentExposure

identifierSource :: Uniform TestEnvironment Int32
identifierSource = uniformSource "identifier" environmentIdentifier

albedoSource :: Texture TestEnvironment
albedoSource = textureSource "albedo" environmentTexture

swapchainTarget :: ColorTarget TestEnvironment 'R8G8B8A8Srgb
swapchainTarget = colorTarget "swapchain" environmentColor

linearTarget :: ColorTarget TestEnvironment 'R32Sfloat
linearTarget = colorTarget "linear" environmentLinearColor

depthTargetValue :: DepthTarget TestEnvironment 'D32Sfloat
depthTargetValue = depthTarget "depth" environmentDepth

typedTriangle :: PipelineM TestEnvironment ()
typedTriangle = do
  vertices <- vertexInput meshSource
  mvp <- uniform mvpSource
  let projected = fmap (projectColoredVertex mvp) vertices
  fragments <- rasterize defaultRaster projected
  drawColor defaultBlend swapchainTarget (fmap unSmooth fragments)

typedTriangleCase :: IO ()
typedTriangleCase = do
  compiled <- compileSuccessfully typedTriangle
  draw <- expectExactlyOne "compiled draw" (compiledPipelineDraws compiled)
  varying <- expectExactlyOne "compiled varying" (compiledVaryings draw)
  let interface = compiledPipelineInterface compiled
  fmap vertexAttributeName (pipelineVertexAttributes interface) @?= ["mesh.0", "mesh.1"]
  fmap vertexAttributeLocation (pipelineVertexAttributes interface) @?= [0, 1]
  fmap vertexAttributeOffset (pipelineVertexAttributes interface) @?= [0, 12]
  pipelineVertexBindings interface @?= [VertexBindingLayout "mesh" 28]
  compiledDrawTopology draw @?= Triangles
  compiledVaryingLocation varying @?= 0
  compiledVaryingInterpolation varying @?= SmoothInterpolation
  compiledVaryingShaderType varying @?= TyVector 4
  forestContainsInput "vertex.mesh.1" (compiledVertexForest draw) @?= True
  nodeIsBinary (compiledVertexVaryingRoot varying) (compiledVertexForest draw) @?= True
  nodeIsInput "varying.0" (compiledFragmentVaryingRoot varying) (compiledFragmentForest draw) @?= True
  fmap compiledColorTargetName (compiledColorOutputs draw) @?= ["swapchain"]

representativeInterfaceGolden :: IO BL8.ByteString
representativeInterfaceGolden = do
  compiled <- compileSuccessfully representativeInterfacePipeline
  pure (BL8.pack (renderPipelineInterface (compiledPipelineInterface compiled)))

representativeInterfacePipeline :: PipelineM TestEnvironment ()
representativeInterfacePipeline = do
  vertices <- vertexInput meshSource
  mvp <- uniform mvpSource
  exposure <- uniform exposureSource
  albedo <- texture albedoSource
  _ <- storageBuffer (storageSource "instances" environmentStorage)
  _ <- pushConstant environmentPushScalar :: PipelineM TestEnvironment (V Float)
  _ <- pushConstant environmentPushVector :: PipelineM TestEnvironment (F (V4 Float))
  let projected = fmap (\(position, _) -> (projectPosition mvp position, Smooth vertexCenterUv)) vertices
  fragments <- rasterize defaultRaster projected
  let shade (Smooth coordinates) = sample albedo coordinates + vec4 exposure exposure exposure (constant 0)
  drawColor defaultBlend swapchainTarget (fmap shade fragments)
  drawDepth defaultDepth depthTargetValue (fmap (const (constant 0.5)) fragments)

renderPipelineInterface :: PipelineInterface -> String
renderPipelineInterface interface =
  unlines
    ( [ "Pipeline interface"
      , ""
      , "Vertex bindings"
      , "source\tstride"
      ]
        <> fmap renderVertexBinding (pipelineVertexBindings interface)
        <> ["", "Vertex attributes", "source\tattribute\tlocation\tformat\tshader type\toffset"]
        <> fmap renderVertexAttribute (pipelineVertexAttributes interface)
        <> ["", "Resources", "name\tset\tbinding\tkind"]
        <> fmap renderResourceBinding (pipelineResources interface)
        <> ["", "Color attachments", "name\tlocation\tformat"]
        <> fmap renderColorAttachment (pipelineColorAttachments interface)
        <> ["", "Depth attachments", "name\tformat"]
        <> fmap renderDepthAttachment (pipelineDepthAttachments interface)
        <> ["", "Push constants", "name\toffset\tsize\tshader type\tlayout"]
        <> fmap renderPushConstantRange (pipelinePushConstants interface)
    )

renderVertexBinding :: VertexBindingLayout -> String
renderVertexBinding binding = vertexBindingSourceName binding <> "\t" <> show (vertexBindingStride binding)

renderVertexAttribute :: VertexAttribute -> String
renderVertexAttribute attribute =
  vertexAttributeSourceName attribute
    <> "\t"
    <> vertexAttributeName attribute
    <> "\t"
    <> show (vertexAttributeLocation attribute)
    <> "\t"
    <> renderVkFormat (vertexAttributeFormat attribute)
    <> "\t"
    <> renderShaderType (vertexAttributeShaderType attribute)
    <> "\t"
    <> show (vertexAttributeOffset attribute)

renderResourceBinding :: ResourceBinding -> String
renderResourceBinding binding =
  resourceBindingName binding
    <> "\t"
    <> show (resourceBindingSet binding)
    <> "\t"
    <> show (resourceBindingBinding binding)
    <> "\t"
    <> renderResourceShape (resourceBindingShape binding)

renderColorAttachment :: ColorAttachment -> String
renderColorAttachment attachment =
  colorAttachmentName attachment
    <> "\t"
    <> show (colorAttachmentLocation attachment)
    <> "\t"
    <> renderVkFormat (colorAttachmentFormat attachment)

renderDepthAttachment :: DepthAttachment -> String
renderDepthAttachment attachment = depthAttachmentName attachment <> "\t" <> renderVkFormat (depthAttachmentFormat attachment)

renderPushConstantRange :: PushConstantRange -> String
renderPushConstantRange range =
  pushConstantName range
    <> "\t"
    <> show (pushConstantOffset range)
    <> "\t"
    <> show (pushConstantSize range)
    <> "\t"
    <> renderShaderType (pushConstantShaderType range)
    <> "\t"
    <> renderFieldLayout (pushConstantFieldLayout range)

renderResourceShape :: ShaderResourceShape -> String
renderResourceShape shape = case shape of
  UniformShape shaderType layout -> "uniform " <> renderShaderType shaderType <> " " <> renderFieldLayout layout
  CombinedTextureShape dimension -> "texture " <> renderImageDimension dimension
  StorageShape -> "storage"
  StorageArrayShape shaderType layout access -> "storage " <> renderStorageAccess access <> " " <> renderShaderType shaderType <> " " <> renderFieldLayout layout

renderStorageAccess :: StorageAccess -> String
renderStorageAccess access = case access of
  StorageReadOnly -> "read-only"
  StorageWriteOnly -> "write-only"
  StorageReadWrite -> "read-write"
  StorageAtomic -> "atomic"

renderImageDimension :: ImageDimension -> String
renderImageDimension dimension = case dimension of
  Image1D -> "1d"
  Image2D -> "2d"
  Image3D -> "3d"
  ImageCube -> "cube"
  Image2DArray -> "2d-array"

renderShaderType :: ShaderTy -> String
renderShaderType shaderType = case shaderType of
  TyFloat -> "float"
  TyInt -> "int"
  TyWord -> "uint"
  TyBool -> "bool"
  TyVector channels -> "vec" <> show channels
  TyWordVector channels -> "uvec" <> show channels
  TyMatrix columns rows -> "mat" <> show columns <> "x" <> show rows
  TyImage1D -> "image1d"
  TyImage2D -> "image2d"
  TyImage3D -> "image3d"
  TyImageCube -> "image-cube"
  TyImage2DArray -> "image2d-array"
  TySampler -> "sampler"

renderFieldLayout :: FieldLayout -> String
renderFieldLayout field = case field of
  Scalar scalar -> renderScalarType scalar
  Vector channels scalar -> "vec" <> show channels <> "<" <> renderScalarType scalar <> ">"
  Matrix columns rows scalar -> "mat" <> show columns <> "x" <> show rows <> "<" <> renderScalarType scalar <> ">"
  Array count element -> "array[" <> show count <> "]<" <> renderFieldLayout element <> ">"
  Struct fields -> "struct<" <> joinWith ", " (fmap renderFieldLayout fields) <> ">"

renderScalarType :: ScalarType -> String
renderScalarType scalar = case scalar of
  Float32 -> "float32"
  SignedInt32 -> "int32"
  UnsignedInt32 -> "uint32"
  Boolean32 -> "bool32"

renderVkFormat :: Vk.Format -> String
renderVkFormat format
  | format == Vk.FORMAT_R8_UNORM = "R8_UNORM"
  | format == Vk.FORMAT_R8G8B8A8_UNORM = "R8G8B8A8_UNORM"
  | format == Vk.FORMAT_R8G8B8A8_SRGB = "R8G8B8A8_SRGB"
  | format == Vk.FORMAT_B8G8R8A8_UNORM = "B8G8R8A8_UNORM"
  | format == Vk.FORMAT_B8G8R8A8_SRGB = "B8G8R8A8_SRGB"
  | format == Vk.FORMAT_R32_SFLOAT = "R32_SFLOAT"
  | format == Vk.FORMAT_R32G32_SFLOAT = "R32G32_SFLOAT"
  | format == Vk.FORMAT_R32G32B32_SFLOAT = "R32G32B32_SFLOAT"
  | format == Vk.FORMAT_R32G32B32A32_SFLOAT = "R32G32B32A32_SFLOAT"
  | format == Vk.FORMAT_D32_SFLOAT = "D32_SFLOAT"
  | otherwise = "unrecognized-vpipe-format"

joinWith :: String -> [String] -> String
joinWith _ [] = ""
joinWith _ [value] = value
joinWith separator (value : values) = value <> separator <> joinWith separator values

tupleAndZipPipeline :: PipelineM TestEnvironment ()
tupleAndZipPipeline = do
  positions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
  coordinates <- vertexInput (coordinateSource :: VertexSource TestEnvironment 'Triangles (V2 Float))
  mvp <- uniform mvpSource
  let attributes = zipStreams positions coordinates
      projected = fmap (\(position, uv) -> (projectPosition mvp position, (Smooth uv, Flat vertexWhite))) attributes
  fragments <- rasterize defaultRaster projected
  drawColor defaultBlend swapchainTarget (fmap (\(Smooth _, Flat color) -> color) fragments)

tupleAndZipCase :: IO ()
tupleAndZipCase = do
  compiled <- compileSuccessfully tupleAndZipPipeline
  draw <- expectExactlyOne "compiled draw" (compiledPipelineDraws compiled)
  firstVarying <- case compiledVaryings draw of
    varying : _ -> pure varying
    [] -> assertFailure "expected at least one compiled varying"
  let interface = compiledPipelineInterface compiled
  fmap vertexAttributeName (pipelineVertexAttributes interface) @?= ["position", "coordinates"]
  fmap vertexAttributeLocation (pipelineVertexAttributes interface) @?= [0, 1]
  fmap compiledVaryingLocation (compiledVaryings draw) @?= [0, 1]
  fmap compiledVaryingInterpolation (compiledVaryings draw) @?= [SmoothInterpolation, FlatInterpolation]
  nodeIsInput "vertex.coordinates" (compiledVertexVaryingRoot firstVarying) (compiledVertexForest draw) @?= True

integerFlatPipeline :: PipelineM TestEnvironment ()
integerFlatPipeline = do
  positions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
  mvp <- uniform mvpSource
  identifier <- uniform identifierSource
  fragments <- rasterize defaultRaster (fmap (\position -> (projectPosition mvp position, identifier :: V Int32)) positions)
  drawColor defaultBlend swapchainTarget (fmap (const (constant (V4 0 1 0 1 :: V4 Float))) fragments)

integerFlatCase :: IO ()
integerFlatCase = do
  compiled <- compileSuccessfully integerFlatPipeline
  draw <- expectExactlyOne "compiled draw" (compiledPipelineDraws compiled)
  varying <- expectExactlyOne "compiled varying" (compiledVaryings draw)
  compiledVaryingInterpolation varying @?= FlatInterpolation
  nodeIsInput "uniform.identifier" (compiledVertexVaryingRoot varying) (compiledVertexForest draw) @?= True

structuredOutputsPipeline :: PipelineM TestEnvironment ()
structuredOutputsPipeline = do
  positions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
  let projected = fmap (\position -> (vec4 (x position) (y position) (z position) (constant 1), Smooth (x position))) positions
  fragments <- rasterize defaultRaster projected
  let shaded = discardWhen (unSmooth (fragmentValue fragments) <. constant 0) (fmap (const (constant (V4 1 0 1 1 :: V4 Float))) fragments)
      depthStream = writeDepth (constant 0.75) (fmap (const (constant 0.25)) shaded)
  drawColor defaultBlend swapchainTarget shaded
  drawDepth defaultDepth depthTargetValue depthStream

structuredOutputsCase :: IO ()
structuredOutputsCase = do
  compiled <- compileSuccessfully structuredOutputsPipeline
  draw <- expectExactlyOne "compiled draw" (compiledPipelineDraws compiled)
  colorOutput <- expectExactlyOne "compiled color output" (compiledColorOutputs draw)
  compiledColorTargetName colorOutput @?= "swapchain"
  compiledColorLocation colorOutput @?= 0
  fmap compiledDepthTargetName (compiledDepthOutput draw) @?= Just "depth"
  fmap (rootLiteral (compiledFragmentForest draw) . compiledDepthRoot) (compiledDepthOutput draw) @?= Just (Just (HFloat 0.75))
  length (compiledDiscardPredicates draw) @?= 2

multipleDrawsPipeline :: PipelineM TestEnvironment ()
multipleDrawsPipeline = do
  trianglePositions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
  triangleFragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, Smooth vertexRed)) trianglePositions)
  drawColor defaultBlend swapchainTarget (fmap unSmooth triangleFragments)
  linePositions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Lines (V3 Float))
  lineFragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, NoPerspective (x position))) linePositions)
  drawColor defaultBlend linearTarget (fmap unNoPerspective lineFragments)

multipleDrawsCase :: IO ()
multipleDrawsCase = do
  compiled <- compileSuccessfully multipleDrawsPipeline
  fmap compiledDrawTopology (compiledPipelineDraws compiled) @?= [Triangles, Lines]
  fmap (fmap compiledColorTargetName . compiledColorOutputs) (compiledPipelineDraws compiled) @?= [["swapchain"], ["linear"]]

colorLocationsCase :: IO ()
colorLocationsCase = do
  compiled <- compileSuccessfully $ do
    first <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
    firstFragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, Smooth vertexRed)) first)
    drawColor defaultBlend swapchainTarget (fmap unSmooth firstFragments)
    second <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
    secondFragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, Smooth vertexRed)) second)
    drawColor defaultBlend linearTarget (fmap (const (constant (1 :: Float))) secondFragments)
    third <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
    thirdFragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, Smooth vertexRed)) third)
    drawColor defaultBlend swapchainTarget (fmap unSmooth thirdFragments)
    drawColor defaultBlend (colorTarget "alternate" environmentAlternateColor) (fmap unSmooth thirdFragments)
  fmap (fmap compiledColorLocation . compiledColorOutputs) (compiledPipelineDraws compiled) @?= [[0], [0], [0, 1]]
  resolvePipelineBindings compiled testEnvironment
    @?= Right
      ( ResolvedBindingPlan
          { resolvedVertexBuffers = [ResolvedVertexBuffer "position" [0] (RuntimeHandle 11)]
          , resolvedIndexBuffers = []
          , resolvedUniformBuffers = []
          , resolvedStorageBuffers = []
          , resolvedTextures = []
          , resolvedColorImages =
              [ ResolvedColorImage "swapchain" 0 0 (RuntimeHandle 40)
              , ResolvedColorImage "linear" 1 0 (RuntimeHandle 41)
              , ResolvedColorImage "swapchain" 2 0 (RuntimeHandle 40)
              , ResolvedColorImage "alternate" 2 1 (RuntimeHandle 42)
              ]
          , resolvedDepthImages = []
          }
      )

colorOutputCase :: IO ()
colorOutputCase = do
  compiled <- compileSuccessfully $ do
    positions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
    fragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, Smooth vertexRed)) positions)
    drawColor defaultBlend linearTarget (fmap (const (constant (1 :: Float))) fragments)
    drawGenericColor swapchainTarget (fmap unSmooth fragments)
  draw <- expectExactlyOne "compiled draw" (compiledPipelineDraws compiled)
  fmap compiledColorTargetName (compiledColorOutputs draw) @?= ["linear", "swapchain"]

drawGenericColor :: forall format. (Blendable format, ColorRenderable format, KnownFormat format, ColorOutputMatches format (V4 Float)) => ColorTarget TestEnvironment format -> FragmentStream (F (V4 Float)) -> PipelineM TestEnvironment ()
drawGenericColor = drawColor defaultBlend

discardOrderCase :: IO ()
discardOrderCase = do
  colorThenDepth <- compileSuccessfully (discardPipeline True)
  depthThenColor <- compileSuccessfully (discardPipeline False)
  fmap (length . compiledDiscardPredicates) (compiledPipelineDraws colorThenDepth) @?= [2]
  fmap (length . compiledDiscardPredicates) (compiledPipelineDraws depthThenColor) @?= [2]

data RecordVertex = RecordVertex {recordPosition :: V3 Float, recordUv :: V2 Float}
  deriving (Generic)

genericVertexCase :: IO ()
genericVertexCase = do
  compiled <- compileSuccessfully $ do
    vertices <- vertexInput (vertexSource "record" (const (VertexBuffer (RuntimeHandle 60))) :: VertexSource TestEnvironment 'Triangles (GenericVertex RecordVertex))
    fragments <- rasterize defaultRaster (fmap (bimap positionClip Smooth) vertices)
    drawColor defaultBlend swapchainTarget (fmap (\(Smooth uv) -> vec4 (x uv) (y uv) (constant 0) (constant 1)) fragments)
  fmap vertexAttributeName (pipelineVertexAttributes (compiledPipelineInterface compiled)) @?= ["record.0", "record.1"]
  fmap vertexAttributeOffset (pipelineVertexAttributes (compiledPipelineInterface compiled)) @?= [0, 12]
  pipelineVertexBindings (compiledPipelineInterface compiled) @?= [VertexBindingLayout "record" 20]

resourcePipeline :: PipelineM TestEnvironment ()
resourcePipeline = do
  vertices <- vertexInput meshSource
  mvp <- uniform mvpSource
  exposure <- uniform exposureSource
  albedo <- texture albedoSource
  let projected = fmap (\(position, _) -> (projectPosition mvp position, Smooth vertexCenterUv)) vertices
  fragments <- rasterize defaultRaster projected
  let shade (Smooth coordinates) = sample albedo coordinates + vec4 exposure exposure exposure (constant 0)
  drawColor defaultBlend swapchainTarget (fmap shade fragments)

resourceBindingsCase :: IO ()
resourceBindingsCase = do
  first <- compileSuccessfully resourcePipeline
  second <- compileSuccessfully resourcePipeline
  let firstBindings = pipelineResources (compiledPipelineInterface first)
      secondBindings = pipelineResources (compiledPipelineInterface second)
  firstBindings @?= secondBindings
  compiledPipelineDraws first @?= compiledPipelineDraws second
  fmap resourceBindingName firstBindings @?= ["mvp", "exposure", "albedo"]
  fmap resourceBindingBinding firstBindings @?= [0, 1, 2]
  fmap resourceBindingSet firstBindings @?= [0, 0, 0]
  fmap resourceBindingKind firstBindings @?= [UniformResource, UniformResource, TextureResource]
  fmap resourceBindingShape firstBindings
    @?= [ UniformShape (TyMatrix 4 4) (Matrix 4 4 Float32)
        , UniformShape TyFloat (Scalar Float32)
        , CombinedTextureShape Image2D
        ]

sampledTextureDimensionCase :: IO ()
sampledTextureDimensionCase = do
  compiled <- compileSuccessfully $ do
    _ <- sampledTexture (sampledTextureSource "volume" (const (error "metadata-only texture accessor")) :: SampledTexture TestEnvironment 'D3 'R8G8B8A8Unorm)
    pure ()
  fmap resourceBindingShape (pipelineResources (compiledPipelineInterface compiled))
    @?= [CombinedTextureShape Image3D]

accessorResolutionCase :: IO ()
accessorResolutionCase = do
  compiled <- compileSuccessfully resourcePipeline
  resolvePipelineBindings compiled testEnvironment
    @?= Right
      ( ResolvedBindingPlan
          { resolvedVertexBuffers = [ResolvedVertexBuffer "mesh" [0, 1] (RuntimeHandle 10)]
          , resolvedIndexBuffers = []
          , resolvedUniformBuffers = [ResolvedUniformBuffer "mvp" 0 0 (RuntimeHandle 20), ResolvedUniformBuffer "exposure" 0 1 (RuntimeHandle 21)]
          , resolvedStorageBuffers = []
          , resolvedTextures = [ResolvedTexture "albedo" 0 2 (RuntimeHandle 30) (RuntimeHandle 31)]
          , resolvedColorImages = [ResolvedColorImage "swapchain" 0 0 (RuntimeHandle 40)]
          , resolvedDepthImages = []
          }
      )

storageResolutionCase :: IO ()
storageResolutionCase = do
  compiled <- compileSuccessfully $ do
    _ <- uniform (uniformSource "read-only" environmentExposure)
    _ <- storageBuffer (storageSource "writable" environmentStorage)
    pure ()
  fmap resourceBindingKind (pipelineResources (compiledPipelineInterface compiled))
    @?= [UniformResource, StorageResource]
  resolvePipelineBindings compiled testEnvironment
    @?= Right
      ( ResolvedBindingPlan
          { resolvedVertexBuffers = []
          , resolvedIndexBuffers = []
          , resolvedUniformBuffers = [ResolvedUniformBuffer "read-only" 0 0 (RuntimeHandle 21)]
          , resolvedStorageBuffers = [ResolvedStorageBuffer "writable" 0 1 (RuntimeHandle 23)]
          , resolvedTextures = []
          , resolvedColorImages = []
          , resolvedDepthImages = []
          }
      )
  aliased <- compileSuccessfully $ do
    _ <- uniform (uniformSource "read-only" environmentExposure)
    _ <- storageBuffer (storageSource "writable" (const (StorageBuffer (RuntimeHandle 21))))
    pure ()
  resolvePipelineBindings aliased testEnvironment @?= Left (RuntimeResourceAlias (RuntimeHandle 21))

managedBufferAliasOwnershipCase :: IO ()
managedBufferAliasOwnershipCase = do
  owner <- newUnique
  foreignOwner <- newUnique
  firstGeneration <- Lifetime.newResourceGeneration
  replacementGeneration <- Lifetime.newResourceGeneration
  state <- BufferState.newBufferState
  let metadata =
        Resource.BufferBindingMetadata
          { Resource.bufferBindingRawHandle = Handles.Buffer 0xA11A5
          , Resource.bufferBindingState = state
          , Resource.bufferBindingElementCount = 1
          , Resource.bufferBindingStride = 4
          , Resource.bufferBindingByteOffset = 0
          , Resource.bufferBindingUsage = BufferUsage.BUFFER_USAGE_UNIFORM_BUFFER_BIT .|. BufferUsage.BUFFER_USAGE_STORAGE_BUFFER_BIT
          }
      managed managedOwner generation =
        Resource.managedBufferRuntimeHandle managedOwner generation (pure (pure ())) metadata
      original = managed owner firstGeneration
      replacement = managed owner replacementGeneration
      foreignBuffer = managed foreignOwner firstGeneration
      plan uniformHandle storageHandle =
        BindingPlan
          [ ResolveUniformBuffer (const (ResolvedUniformBuffer "read-only" 0 0 uniformHandle))
          , ResolveStorageBuffer (const (ResolvedStorageBuffer "writable" 0 1 storageHandle))
          ]
  resolveBindingPlan (plan original replacement) ()
    @?= Left (RuntimeResourceAlias replacement)
  resolveBindingPlan (plan original foreignBuffer) ()
    @?= Right
      ( ResolvedBindingPlan
          { resolvedVertexBuffers = []
          , resolvedIndexBuffers = []
          , resolvedUniformBuffers = [ResolvedUniformBuffer "read-only" 0 0 original]
          , resolvedStorageBuffers = [ResolvedStorageBuffer "writable" 0 1 foreignBuffer]
          , resolvedTextures = []
          , resolvedColorImages = []
          , resolvedDepthImages = []
          }
      )

runtimeHandleQuarantineCase :: IO ()
runtimeHandleQuarantineCase = do
  owner <- newUnique
  generation <- Lifetime.newResourceGeneration
  observedMaskingState <- newEmptyMVar
  let managed =
        Resource.managedRuntimeHandleWithQuarantine
          owner
          generation
          0x5151
          (pure (pure ()))
          (getMaskingState >>= putMVar observedMaskingState)
  Resource.runtimeHandleQuarantine (RuntimeHandle 0x5150)
  Resource.runtimeHandleQuarantine managed
  takeMVar observedMaskingState >>= (@?= MaskedInterruptible)

pushConstantCase :: IO ()
pushConstantCase = do
  compiled <- compileSuccessfully $ do
    _ <- pushConstant environmentPushScalar :: PipelineM TestEnvironment (V Float)
    _ <- pushConstant environmentPushVector :: PipelineM TestEnvironment (F (V4 Float))
    _ <- pushConstant (const (toMatrixBuffer (V2 (V3 1 2 3) (V3 4 5 6)))) :: PipelineM TestEnvironment (V (V2 (V3 Float)))
    pure ()
  let ranges = pipelinePushConstants (compiledPipelineInterface compiled)
  fmap pushConstantName ranges @?= ["push.0", "push.1", "push.2"]
  fmap pushConstantOffset ranges @?= [0, 16, 32]
  fmap pushConstantSize ranges @?= [4, 16, 24]
  fmap pushConstantShaderType ranges @?= [TyFloat, TyVector 4, TyMatrix 2 3]
  fmap pushConstantFieldLayout ranges @?= [Scalar Float32, Vector 4 Float32, Matrix 3 2 Float32]
  resolved <- resolvePipelinePushConstants compiled testEnvironment
  fmap resolvedPushConstantName resolved @?= ["push.0", "push.1", "push.2"]
  fmap resolvedPushConstantOffset resolved @?= [0, 16, 32]
  fmap (ByteString.length . resolvedPushConstantBytes) resolved @?= [4, 16, 24]
  matrixBytes <- floatsIn (resolvedPushConstantBytes (resolved !! 2))
  matrixBytes @?= [1, 4, 2, 5, 3, 6]

pushConstantTotalLimitCase :: IO ()
pushConstantTotalLimitCase = do
  result <- compilePipeline $ do
    _ <- pushConstant (const (toMatrixBuffer matrixIdentity)) :: PipelineM TestEnvironment (V (M44 Float))
    _ <- pushConstant (const (toMatrixBuffer matrixIdentity)) :: PipelineM TestEnvironment (F (M44 Float))
    _ <- pushConstant (const (toMatrixBuffer matrixIdentity)) :: PipelineM TestEnvironment (V (M44 Float))
    pure ()
  case result of
    Left (PushConstantTotalSizeExceeded 192) -> pure ()
    other -> assertFailure ("expected PushConstantTotalSizeExceeded 192, received " <> describeCompilation other)
 where
  matrixIdentity = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)

floatsIn :: ByteString.ByteString -> IO [Float]
floatsIn bytes = ByteString.useAsCString bytes $ \pointer -> traverse (peekByteOff pointer) [0, 4, 8, 12, 16, 20]

booleanBufferMetadataCase :: IO ()
booleanBufferMetadataCase = do
  uniformResult <- compilePipeline $ do
    _ <- uniform (uniformSource "flag" (const (UniformBuffer (RuntimeHandle 72))) :: Uniform TestEnvironment Bool)
    pure ()
  pushResult <- compilePipeline $ do
    _ <- pushConstant (const True) :: PipelineM TestEnvironment (V Bool)
    pure ()
  expectBooleanMetadataError uniformResult
  expectBooleanMetadataError pushResult

expectBooleanMetadataError :: Either PipelineError a -> IO ()
expectBooleanMetadataError result = case result of
  Left UnsupportedBooleanBufferLayout{} -> pure ()
  Left errorValue -> assertFailure ("expected UnsupportedBooleanBufferLayout, received " <> show errorValue)
  Right _ -> assertFailure "expected UnsupportedBooleanBufferLayout, but compilation succeeded"

resourceCollisionCase :: IO ()
resourceCollisionCase = do
  result <- compilePipeline $ do
    _ <- uniform (uniformSource "shared" environmentExposure)
    _ <- texture (textureSource "shared" environmentTexture)
    pure ()
  case result of
    Left ResourceConflict{} -> pure ()
    other -> assertFailure ("expected ResourceConflict, received " <> describeCompilation other)

vertexCollisionCase :: IO ()
vertexCollisionCase = do
  result <- compilePipeline $ do
    _ <- vertexInput (vertexSource "shared" environmentPositions :: VertexSource TestEnvironment 'Triangles (V3 Float))
    _ <- vertexInput (vertexSource "shared" environmentCoordinates :: VertexSource TestEnvironment 'Triangles (V2 Float))
    pure ()
  case result of
    Left VertexSourceConflict{} -> pure ()
    other -> assertFailure ("expected VertexSourceConflict, received " <> describeCompilation other)

attachmentCollisionCase :: IO ()
attachmentCollisionCase = do
  result <- compilePipeline $ do
    trianglePositions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
    triangleFragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, Smooth vertexRed)) trianglePositions)
    drawColor defaultBlend (colorTarget "shared" environmentColor) (fmap unSmooth triangleFragments)
    linePositions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Lines (V3 Float))
    lineFragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, Smooth (x position))) linePositions)
    drawColor defaultBlend (colorTarget "shared" environmentLinearColor) (fmap unSmooth lineFragments)
  case result of
    Left ColorTargetConflict{} -> pure ()
    other -> assertFailure ("expected ColorTargetConflict, received " <> describeCompilation other)

accessorCollisionCase :: IO ()
accessorCollisionCase = do
  sharedSource <- compileSuccessfully $ do
    _ <- vertexInput (vertexSource "same-source" environmentPositions :: VertexSource TestEnvironment 'Triangles (V3 Float))
    _ <- vertexInput (vertexSource "same-source" environmentPositions :: VertexSource TestEnvironment 'Triangles (V3 Float))
    pure ()
  vertex <- compileSuccessfully $ do
    _ <- vertexInput (vertexSource "shared" environmentPositions :: VertexSource TestEnvironment 'Triangles (V3 Float))
    _ <- vertexInput (vertexSource "shared" (const (VertexBuffer (RuntimeHandle 99))) :: VertexSource TestEnvironment 'Triangles (V3 Float))
    pure ()
  uniformPipeline <- compileSuccessfully $ do
    _ <- uniformSourceValue environmentExposure
    _ <- uniformSourceValue environmentMvpFloat
    pure ()
  target <- compileSuccessfully $ do
    positions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
    fragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, Smooth vertexRed)) positions)
    drawColor defaultBlend (colorTarget "shared-target" environmentColor) (fmap unSmooth fragments)
    drawColor defaultBlend (colorTarget "shared-target" environmentAlternateColor) (fmap unSmooth fragments)
  resolvePipelineBindings sharedSource testEnvironment @?= Right (ResolvedBindingPlan [ResolvedVertexBuffer "same-source" [0] (RuntimeHandle 11)] [] [] [] [] [] [])
  accessorConflictName (resolvePipelineBindings vertex testEnvironment) @?= Just "shared"
  accessorConflictName (resolvePipelineBindings uniformPipeline testEnvironment) @?= Just "shared-uniform"
  accessorConflictName (resolvePipelineBindings target testEnvironment) @?= Just "shared-target"
 where
  uniformSourceValue accessor = uniform (uniformSource "shared-uniform" accessor)
  environmentMvpFloat _ = UniformBuffer (RuntimeHandle 98) :: UniformBuffer Float

accessorConflictName :: Either PipelineError a -> Maybe String
accessorConflictName result = case result of
  Left (AccessorConflict name) -> Just name
  _ -> Nothing

indexedDrawCase :: IO ()
indexedDrawCase = do
  let plan = BindingPlan [ResolveIndexBuffer (const (ResolvedIndexBuffer "indices" (RuntimeHandle 13))), ResolveIndexBuffer (const (ResolvedIndexBuffer "indices" (RuntimeHandle 13)))]
  resolvedIndexBuffers <$> resolveBindingPlan plan testEnvironment @?= Right [ResolvedIndexBuffer "indices" (RuntimeHandle 13)]

discardPipeline :: Bool -> PipelineM TestEnvironment ()
discardPipeline colorFirst = do
  positions <- vertexInput (positionSource :: VertexSource TestEnvironment 'Triangles (V3 Float))
  fragments <- rasterize defaultRaster (fmap (\position -> (positionClip position, Smooth (x position))) positions)
  let color = discardWhen (unSmooth (fragmentValue fragments) <. constant 0) (fmap (const (constant (V4 1 0 0 1 :: V4 Float))) fragments)
      depth = discardWhen (unSmooth (fragmentValue fragments) >. constant 0) (fmap (const (constant 0.5)) fragments)
  if colorFirst
    then drawColor defaultBlend swapchainTarget color >> drawDepth defaultDepth depthTargetValue depth
    else drawDepth defaultDepth depthTargetValue depth >> drawColor defaultBlend swapchainTarget color

projectPosition :: V (M44 Float) -> V (V3 Float) -> V (V4 Float)
projectPosition mvp position = mvp !* positionClip position

projectColoredVertex :: V (M44 Float) -> (V (V3 Float), V (V4 Float)) -> (V (V4 Float), Smooth 'Vertex (V4 Float))
projectColoredVertex mvp (position, color) = (projectPosition mvp position, Smooth (color + vertexRed))

positionClip :: V (V3 Float) -> V (V4 Float)
positionClip position = vec4 (x position) (y position) (z position) (constant 1)

vertexRed :: V (V4 Float)
vertexRed = constant (V4 1 0 0 1)

vertexWhite :: V (V4 Float)
vertexWhite = constant (V4 1 1 1 1)

vertexCenterUv :: V (V2 Float)
vertexCenterUv = constant (V2 0.5 0.5)

compileSuccessfully :: PipelineM env () -> IO (CompiledPipeline env)
compileSuccessfully pipeline = do
  result <- compilePipeline pipeline
  case result of
    Left pipelineError -> assertFailure ("pipeline compilation failed: " <> show pipelineError)
    Right compiled -> pure compiled

forestContainsInput :: String -> ReifiedForest -> Bool
forestContainsInput name forest = any isRequestedInput (forestNodes forest)
 where
  isRequestedInput node = reifiedOp node == RInput name

nodeIsInput :: String -> NodeId -> ReifiedForest -> Bool
nodeIsInput name identifier forest = case findNode identifier (forestNodes forest) of
  Just node -> reifiedOp node == RInput name
  Nothing -> False

nodeIsBinary :: NodeId -> ReifiedForest -> Bool
nodeIsBinary identifier forest = case findNode identifier (forestNodes forest) of
  Just node -> case reifiedOp node of
    RBinary{} -> True
    _ -> False
  Nothing -> False

rootLiteral :: ReifiedForest -> NodeId -> Maybe HostValue
rootLiteral forest identifier = case findNode identifier (forestNodes forest) of
  Just node -> case reifiedOp node of
    RLiteral value -> Just value
    _ -> Nothing
  Nothing -> Nothing

findNode :: NodeId -> [ReifiedNode] -> Maybe ReifiedNode
findNode = findByIdentifier
 where
  findByIdentifier _ [] = Nothing
  findByIdentifier identifier (node : rest)
    | reifiedId node == identifier = Just node
    | otherwise = findByIdentifier identifier rest

describeCompilation :: Either PipelineError (CompiledPipeline env) -> String
describeCompilation result = case result of
  Left pipelineError -> show pipelineError
  Right _ -> "successful compilation"

expectExactlyOne :: String -> [a] -> IO a
expectExactlyOne description values = case values of
  [value] -> pure value
  _ -> assertFailure ("expected exactly one " <> description <> ", received " <> show (length values))
