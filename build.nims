# Harding build script

import os, strutils

# Build and stage binaries
task "setup", "Build and copy binaries to root directory":
  exec "nimble build"
  # Copy binaries to root directory for convenience
  exec "cp harding/repl/harding harding 2>/dev/null || true"
  exec "cp harding/compiler/granite granite 2>/dev/null || true"
  echo "Binaries available in root directory as harding and granite"

# Build tests
task "test", "Run tests":
  exec "nimble test"

# Clean build artifacts
task "clean", "Clean build artifacts":
  exec "rm -rf nimcache build 2>/dev/null || true"
  # Clean binaries in various possible locations
  exec "rm -f harding granite bona harding.exe granite.exe bona.exe 2>/dev/null || true"
  exec "rm -f harding/repl/harding harding/compiler/granite harding/gui/ide 2>/dev/null || true"

# Install binary
task "install", "Install Harding":
  # Check multiple possible binary locations
  var binPath = ""
  for possiblePath in [
    getCurrentDir() / "harding",
    "harding/repl/harding"
  ]:
    if fileExists(possiblePath):
      binPath = possiblePath
      break

  when defined(windows):
    if binPath == "":
      for possiblePath in [
        getCurrentDir() / "harding.exe",
        "harding/repl/harding.exe"
      ]:
        if fileExists(possiblePath):
          binPath = possiblePath
          break

  if binPath == "":
    echo "Error: harding binary not found. Run 'nimble build' first."
    echo "Checked locations: ./harding, harding/repl/harding"
    return

  let dest = getHomeDir() / ".local" / "bin" / "harding"
  when defined(windows):
    # On Windows, install to a common location
    let winDest = getHomeDir() / "harding" / "harding.exe"
    exec "mkdir -p " & (getHomeDir() / "harding")
    exec "cp " & binPath & " " & winDest
    echo "Installed to: " & winDest
  else:
    exec "cp " & binPath & " " & dest & " && chmod +x " & dest
    echo "Installed to: " & dest
