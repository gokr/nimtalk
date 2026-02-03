# Nemo - Modern Smalltalk dialect
version = "0.2.0"
author = "Göran Krampe"
description = "Modern Smalltalk dialect written in Nim"
license = "MIT"

srcDir = "src"
bin = @["nemo/repl/nemo", "nemo/compiler/nemoc"]

# Current Nim version
requires "nim == 2.2.6"

# FFI dependencies
when defined(linux):
  requires "libffi"
when defined(macosx):
  passL "-ldl"

import os, strutils

task test, "Run all tests (automatic discovery via testament)":
  exec """
    echo "Running Nemo test suite..."
    echo "=== Running tests/test_*.nim ==="
    testament pattern "tests/test_*.nim" || true
    # echo "=== Running tests/category/*.nim ==="
    # testament pattern "tests/**/*.nim" || true
    # echo "=== Running tests/category/subcategory/*.nim ==="
    # testament pattern "tests/**/**/*.nim" || true
  """

task local, "Build and copy binaries to root directory":
  # Build REPL directly (nimble build has path conflicts with package name)
  exec "nim c -o:nemo src/nemo/repl/nemo.nim"
  exec "nim c -o:nemoc src/nemo/compiler/nemoc.nim"
  echo "Binaries available in root directory as nemo and nemoc"

task gui, "Build the GUI IDE":
  # Build the GUI IDE with GTK4
  exec "nim c -o:nemo-ide src/nemo/gui/ide.nim"
  echo "GUI IDE binary available as nemo-ide"

task clean, "Clean build artifacts using build.nims":
  exec "nim e build.nims clean"

task install, "Install nemo to ~/.local/bin/":
  var binPath = ""
  for possiblePath in ["nemo", "nemo/repl/nemo"]:
    if possiblePath.fileExists:
      binPath = possiblePath
      break

  when defined(windows):
    if binPath == "":
      for possiblePath in ["nemo.exe", "nemo/repl/nemo.exe"]:
        if possiblePath.fileExists:
          binPath = possiblePath
          break

  if binPath == "" or not binPath.fileExists:
    echo "Error: nemo binary not found. Run 'nimble build' or 'nimble setup' first."
    echo "Checked locations: ./nemo, nemo/repl/nemo"
    system.quit(1)

  when defined(windows):
    let dest = getHomeDir() / "nemo" / "nemo.exe"
    exec "mkdir -p " & (getHomeDir() / "nemo")
    exec "cp " & binPath & " " & dest
    echo "Installed to: " & dest
  else:
    let dest = getHomeDir() / ".local" / "bin" / "nemo"
    exec "mkdir -p " & (getHomeDir() / ".local" / "bin")
    exec "cp " & binPath & " " & dest & " && chmod +x " & dest
    echo "Installed to: " & dest
