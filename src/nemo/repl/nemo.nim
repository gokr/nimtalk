#!/usr/bin/env nim
#
# Nemo REPL - Main entry point
#

import std/[os, strutils, logging]
import ../repl/doit
import ../repl/cli
import ../core/types

# ============================================================================
# Main entry point for Nemo REPL
# ============================================================================

const
  AppName = "nemo"
  AppDesc = "Nemo REPL - Modern Smalltalk in Nim"

let Examples = @[
  "nemo                              # Start REPL interactively",
  "nemo examples/demo.nemo           # Run a script file",
  "nemo -e \"3 + 4\"                  # Evaluate expression (prints: 7)",
  "nemo --ast examples/test.nemo     # Show AST then execute",
  "nemo --loglevel DEBUG script.nemo # Run with debug logging",
  "nemo --home /opt/nemo             # Use custom home directory",
  "nemo --bootstrap custom.nemo      # Use custom bootstrap file"
]

proc runTests(maxStackDepth: int): int =
  ## Run built-in tests, returns exit code
  echo "Running Nemo tests..."
  var passed, failed = 0

  # Test 1: Basic arithmetic (3 + 4 = 7)
  let (t1ok, t1msg) = testREPL()
  if t1ok:
    inc passed
    echo "✓ Test 1: Basic REPL functionality"
  else:
    inc failed
    echo "✗ Test 1: " & t1msg

  # Test 2: Expression evaluation
  try:
    var ctx = newDoitContext(maxStackDepth = maxStackDepth)
    let (result, err) = ctx.doit("42")
    if err.len == 0 and result.kind == vkInt and result.intVal == 42:
      inc passed
      echo "✓ Test 2: Expression evaluation"
    else:
      inc failed
      echo "✗ Test 2: Expected 42, got: " & result.toString()
  except:
    inc failed
    echo "✗ Test 2: Exception during evaluation"

  # Test 3: Object creation
  try:
    var ctx = newDoitContext(maxStackDepth = maxStackDepth)
    discard ctx.doit("obj := Object clone")
    let (name, err) = ctx.doit("obj printString")
    if err.len == 0:
      inc passed
      echo "✓ Test 3: Object creation and messaging"
    else:
      inc failed
      echo "✗ Test 3: " & err
  except:
    inc failed
    echo "✗ Test 3: Exception during object test"

  # Summary
  echo ""
  echo "Tests: " & $passed & " passed, " & $failed & " failed"
  if failed == 0:
    echo "All tests passed! ✨"
    return 0
  else:
    echo "Some tests failed. ⚠"
    return 1

proc main() =
  # Parse command line arguments
  let opts = parseCliOptions(commandLineParams(), AppName, AppDesc)

  # Configure logging
  setupLogging(opts.logLevel)

  # Handle help and version first
  if opts.positionalArgs.len == 1:
    case opts.positionalArgs[0]:
    of "--help", "-h":
      showUsage(AppName, AppDesc, Examples)
      quit(0)
    of "--version", "-v":
      echo "Nemo " & VERSION
      quit(0)
    of "--test":
      quit(runTests(opts.maxStackDepth))

  # Set NEMO_HOME environment for child processes
  putEnv("NEMO_HOME", opts.nemoHome)

  # Now handle commands based on positional arguments
  if opts.positionalArgs.len == 0:
    # Start REPL
    var ctx = newDoitContext(maxStackDepth = opts.maxStackDepth, nemoHome = opts.nemoHome,
                             bootstrapFile = opts.bootstrapFile)
    runREPL(ctx)
  elif opts.positionalArgs.len == 1:
    # Check if it's a file
    if fileExists(opts.positionalArgs[0]):
      # Run script file
      execScript(opts.positionalArgs[0], opts.dumpAst, opts.maxStackDepth, opts.nemoHome, opts.bootstrapFile)
    else:
      # Unrecognized argument
      echo "Unknown option or file not found: " & opts.positionalArgs[0]
      echo "Use --help for usage information"
      quit(1)
  elif opts.positionalArgs.len == 2 and opts.positionalArgs[0] == "-e":
    # Evaluate expression
    var ctx = newDoitContext(maxStackDepth = opts.maxStackDepth, nemoHome = opts.nemoHome,
                             bootstrapFile = opts.bootstrapFile)
    let (result, err) = ctx.doit(opts.positionalArgs[1], opts.dumpAst)
    if err.len > 0:
      stderr.writeLine("Error: " & err)
      quit(1)
    else:
      if result.kind != vkNil:
        echo result.toString()
      quit(0)
  else:
    echo "Invalid arguments"
    showUsage(AppName, AppDesc, Examples)
    quit(1)

# Entry point
when isMainModule:
  main()
