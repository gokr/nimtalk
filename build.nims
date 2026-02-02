# Nemo build script

import os, strutils

# Build and stage binaries
task "setup", "Build and copy binaries to root directory":
  exec "nimble build"
  # Copy binaries to root directory for convenience
  exec "cp nemo/repl/nemo nemo 2>/dev/null || true"
  exec "cp nemo/compiler/nemoc nemoc 2>/dev/null || true"
  echo "Binaries available in root directory as nemo and nemoc"

# Build tests
task "test", "Run tests":
  exec "nimble test"

# Clean build artifacts
task "clean", "Clean build artifacts":
  exec "rm -rf nimcache build 2>/dev/null || true"
  # Clean binaries in various possible locations
  exec "rm -f nemo nemoc nemo.exe nemoc.exe 2>/dev/null || true"
  exec "rm -f nemo/repl/nemo nemo/repl/nemoc nemo/compiler/nemo nemo/compiler/nemoc 2>/dev/null || true"

# Install binary
task "install", "Install Nemo":
  # Check multiple possible binary locations
  var binPath = ""
  for possiblePath in [
    getCurrentDir() / "nemo",
    "nemo/repl/nemo"
  ]:
    if fileExists(possiblePath):
      binPath = possiblePath
      break

  when defined(windows):
    if binPath == "":
      for possiblePath in [
        getCurrentDir() / "nemo.exe",
        "nemo/repl/nemo.exe"
      ]:
        if fileExists(possiblePath):
          binPath = possiblePath
          break

  if binPath == "":
    echo "Error: nemo binary not found. Run 'nimble build' first."
    echo "Checked locations: ./nemo, nemo/repl/nemo"
    return

  let dest = getHomeDir() / ".local" / "bin" / "nemo"
  when defined(windows):
    # On Windows, install to a common location
    let winDest = getHomeDir() / "nemo" / "nemo.exe"
    exec "mkdir -p " & (getHomeDir() / "nemo")
    exec "cp " & binPath & " " & winDest
    echo "Installed to: " & winDest
  else:
    exec "cp " & binPath & " " & dest & " && chmod +x " & dest
    echo "Installed to: " & dest
