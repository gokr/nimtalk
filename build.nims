# Nimtalk build script

import os, strutils

# Build and stage binaries
task "setup", "Build and copy binaries to root directory":
  exec "nimble build"
  # Copy binaries to root directory for convenience
  exec "cp nimtalk/repl/ntalk ntalk 2>/dev/null || true"
  exec "cp nimtalk/compiler/ntalkc ntalkc 2>/dev/null || true"
  echo "Binaries available in root directory as ntalk and ntalkc"

# Build tests
task "test", "Run tests":
  exec "nimble test"

# Clean build artifacts
task "clean", "Clean build artifacts":
  exec "rm -rf nimcache build 2>/dev/null || true"
  # Clean binaries in various possible locations
  exec "rm -f ntalk ntalkc ntalk.exe ntalkc.exe 2>/dev/null || true"
  exec "rm -f nimtalk/repl/ntalk nimtalk/repl/ntalkc nimtalk/compiler/ntalk nimtalk/compiler/ntalkc 2>/dev/null || true"

# Install binary
task "install", "Install Nimtalk":
  # Check multiple possible binary locations
  var binPath = ""
  for possiblePath in [
    getCurrentDir() / "ntalk",
    "nimtalk/repl/ntalk"
  ]:
    if fileExists(possiblePath):
      binPath = possiblePath
      break

  when defined(windows):
    if binPath == "":
      for possiblePath in [
        getCurrentDir() / "ntalk.exe",
        "nimtalk/repl/ntalk.exe"
      ]:
        if fileExists(possiblePath):
          binPath = possiblePath
          break

  if binPath == "":
    echo "Error: ntalk binary not found. Run 'nimble build' first."
    echo "Checked locations: ./ntalk, nimtalk/repl/ntalk"
    return

  let dest = getHomeDir() / ".local" / "bin" / "ntalk"
  when defined(windows):
    # On Windows, install to a common location
    let winDest = getHomeDir() / "ntalk" / "ntalk.exe"
    exec "mkdir -p " & (getHomeDir() / "ntalk")
    exec "cp " & binPath & " " & winDest
    echo "Installed to: " & winDest
  else:
    exec "cp " & binPath & " " & dest & " && chmod +x " & dest
    echo "Installed to: " & dest
