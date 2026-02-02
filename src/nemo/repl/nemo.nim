#!/usr/bin/env nim
#
# Nemo REPL - Main entry point
#

import std/[os, strutils, logging]
import ../repl/doit
import ../core/types

# ============================================================================
# Main entry point for Nemo REPL
# ============================================================================

# Version constant
const VERSION* = block:
  const nimblePath = currentSourcePath().parentDir().parentDir().parentDir().parentDir() / "nemo.nimble"
  const nimbleContent = staticRead(nimblePath)
  var versionStr = "unknown"
  for line in nimbleContent.splitLines():
    let trimmed = line.strip()
    if trimmed.startsWith("version"):
      let parts = trimmed.split("=")
      if parts.len >= 2:
        versionStr = parts[1].strip().strip(chars={'\"', '\''})
        break
  versionStr

proc parseLogLevel(levelStr: string): Level =
  ## Parse log level string to Level enum
  case levelStr.toUpperAscii()
  of "DEBUG":
    return lvlDebug
  of "INFO":
    return lvlInfo
  of "WARN", "WARNING":
    return lvlWarn
  of "ERROR":
    return lvlError
  of "FATAL":
    return lvlFatal
  else:
    echo "Invalid log level: ", levelStr
    echo "Valid levels: DEBUG, INFO, WARN, ERROR, FATAL"
    quit(1)

proc showUsage() =
  echo "Nemo REPL - Modern Smalltalk in Nim"
  echo ""
  echo "Usage:"
  echo "  nemo [options]                    # Start interactive REPL"
  echo "  nemo [options] <file.nt>          # Run script file"
  echo "  nemo [options] -e \"<code>\"       # Evaluate expression"
  echo "  nemo --test                       # Run built-in tests"
  echo "  nemo --help                       # Show this help"
  echo "  nemo --version                    # Show version"
  echo ""
  echo "Options:"
  echo "  --ast              Dump AST after parsing and continue execution"
  echo "  --loglevel <level> Set log level: DEBUG, INFO, WARN, ERROR (default: ERROR)"
  echo "  --stack-depth <n>  Set maximum stack depth (default: 10000)"
  echo ""
  echo "Examples:"
  echo "  nemo                              # Start REPL interactively"
  echo "  nemo examples/demo.nt             # Run a script file"
  echo "  nemo -e \"3 + 4\"                 # Evaluate expression (prints: 7)"
  echo "  nemo --ast examples/test.nt       # Show AST then execute"
  echo "  nemo --loglevel DEBUG script.nt   # Run with debug logging"
  echo "  nemo --stack-depth 50000 deep.nt  # Run with increased stack limit"
  echo ""

proc main() =
  # Check command line arguments
  let allArgs = commandLineParams()

  # Configure logging (default to ERROR level)
  var logLevel = lvlError
  var dumpAst = false
  var maxStackDepth = 10000
  var positionalArgs: seq[string] = @[]

  # Parse flags and collect positional arguments
  var i = 0
  while i < allArgs.len:
    case allArgs[i]
    of "--loglevel":
      if i + 1 < allArgs.len:
        logLevel = parseLogLevel(allArgs[i + 1])
        inc i  # Skip the value
      else:
        echo "Error: --loglevel requires a value"
        quit(1)
    of "--stack-depth":
      if i + 1 < allArgs.len:
        try:
          maxStackDepth = parseInt(allArgs[i + 1])
          if maxStackDepth < 100:
            echo "Error: --stack-depth must be at least 100"
            quit(1)
        except ValueError:
          echo "Error: --stack-depth requires an integer value"
          quit(1)
        inc i  # Skip the value
      else:
        echo "Error: --stack-depth requires a value"
        quit(1)
    of "--ast":
      dumpAst = true
    of "--help", "-h", "--version", "-v", "--test":
      # These will be handled later
      positionalArgs.add(allArgs[i])
    else:
      # Collect positional arguments
      positionalArgs.add(allArgs[i])
    inc i

  # Add console logger with specified level
  var consoleLogger = newConsoleLogger()
  consoleLogger.levelThreshold = logLevel
  addHandler(consoleLogger)

  # Now handle commands based on positional arguments
  if positionalArgs.len == 0:
    # Start REPL
    var ctx = newDoitContext(maxStackDepth = maxStackDepth)
    runREPL(ctx)
  elif positionalArgs.len == 1:
    case positionalArgs[0]:
    of "--help", "-h":
      showUsage()
    of "--version", "-v":
      echo "Nemo " & VERSION
    of "--test":
      # Run tests
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
        quit(0)
      else:
        echo "Some tests failed. ⚠"
        quit(1)
    else:
      # Check if it's a file (any extension, not just .nt)
      if fileExists(positionalArgs[0]):
        # Run script file
        execScript(positionalArgs[0], dumpAst, maxStackDepth)
      else:
        # Unrecognized argument
        echo "Unknown option or file not found: " & positionalArgs[0]
        echo "Use --help for usage information"
        quit(1)
  elif positionalArgs.len == 2 and positionalArgs[0] == "-e":
    # Evaluate expression
    var ctx = newDoitContext(maxStackDepth = maxStackDepth)
    let (result, err) = ctx.doit(positionalArgs[1], dumpAst)
    if err.len > 0:
      stderr.writeLine("Error: " & err)
      quit(1)
    else:
      if result.kind != vkNil:
        echo result.toString()
      quit(0)
  else:
    echo "Invalid arguments"
    showUsage()
    quit(1)

# Entry point
when isMainModule:
  main()
