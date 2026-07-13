[CmdletBinding()]
param(
  [Parameter(Mandatory = $true)]
  [string[]]$TestBinaries,

  [Parameter(Mandatory = $true)]
  [string]$ArtifactDirectory
)

$ErrorActionPreference = 'Stop'

New-Item -ItemType Directory -Force -Path $ArtifactDirectory | Out-Null

$attemptLog = Join-Path $ArtifactDirectory 'windows-software-vulkan-attempt.log'
$validationLog = Join-Path $ArtifactDirectory 'validation.log'

@(
  'Windows software Vulkan attempt'
  "msys2-setup=$env:MSYS2_SETUP_OUTCOME"
  "lavapipe-install=$env:LAVAPIPE_INSTALL_OUTCOME"
  "lavapipe-configuration=$env:LAVAPIPE_CONFIGURATION_OUTCOME"
) | Set-Content -Path $attemptLog

$testDevice = 'skip'
if ($env:VPIPE_WINDOWS_LAVAPIPE_READY -eq 'true') {
  $vulkanInfo = Join-Path $env:VPIPE_WINDOWS_LAVAPIPE_BIN 'vulkaninfo.exe'
  if (Test-Path -LiteralPath $vulkanInfo) {
    & $vulkanInfo --summary *>&1 | Tee-Object -FilePath $attemptLog -Append
    $probeExit = $LASTEXITCODE
    if ($probeExit -eq 0) {
      $testDevice = 'any'
      'result=lavapipe-probe-succeeded' | Add-Content -Path $attemptLog
    }
    else {
      "result=lavapipe-probe-failed (exit $probeExit); running pure tests" |
        Add-Content -Path $attemptLog
    }
  }
  else {
    "result=vulkaninfo-not-found ($vulkanInfo); running pure tests" |
      Add-Content -Path $attemptLog
  }
}
else {
  'result=lavapipe-unavailable; running pure tests' | Add-Content -Path $attemptLog
}

$env:VPIPE_TEST_DEVICE = $testDevice
"test-device=$testDevice" | Add-Content -Path $attemptLog

foreach ($testBinary in $TestBinaries) {
  "test-binary=$testBinary" | Add-Content -Path $attemptLog
  & $testBinary --hide-successes *>&1 | Tee-Object -FilePath $validationLog -Append
  $testExit = $LASTEXITCODE
  if ($testExit -ne 0) {
    "result=tests-failed ($testBinary exited $testExit)" | Add-Content -Path $attemptLog
    exit $testExit
  }
}

'result=tests-passed' | Add-Content -Path $attemptLog
