## ============================================================================
## Nemo IDE - Main entry point
## Initializes the interpreter, loads GTK bridge, and launches the IDE
## ============================================================================

import std/[os, strutils, logging, tables]
import nemo/core/types
import nemo/core/scheduler
import nemo/interpreter/evaluator
import nemo/interpreter/objects
import nemo/repl/doit
import nemo/repl/cli
import nemo/gui/gtk4/bridge
import nemo/gui/gtk4/ffi
import nemo/gui/gtk4/widget

const
  AppName = "nemo-ide"
  AppDesc = "Nemo IDE - GTK-based graphical IDE"

proc runIde*(opts: CliOptions) =
  ## Main IDE entry point - initializes interpreter and launches IDE

  echo "Starting Nemo IDE..."
  debug("Initializing Nemo IDE")

  # Set NEMO_HOME environment
  putEnv("NEMO_HOME", opts.nemoHome)

  # Create scheduler context (this also initializes the interpreter)
  var ctx = newSchedulerContext()
  var interp = cast[Interpreter](ctx.mainProcess.interpreter)

  # Set nemoHome on the interpreter
  interp.nemoHome = opts.nemoHome

  debug("Scheduler context created")

  # Load standard library
  loadStdlib(interp, opts.bootstrapFile)
  debug("Standard library loaded")

  # Initialize GTK bridge
  initGtkBridge(interp)
  debug("GTK bridge initialized")

  # Load Nemo-side GTK wrapper files
  loadGtkWrapperFiles(interp)
  debug("GTK wrapper files loaded")

  # Load IDE tool files
  loadIdeToolFiles(interp)
  debug("IDE tool files loaded")

  # Run GTK main loop
  debug("Starting GTK main loop")

  # Launch the IDE by calling Launcher open
  echo "About to call Launcher open..."
  let launchCode = "Launcher open"
  echo "Launch code: ", launchCode
  let (_, err) = interp.evalStatements(launchCode)
  echo "Eval result: err=", err
  if err.len > 0:
    stderr.writeLine("Error launching IDE: ", err)
    quit(1)
  echo "Launcher open completed successfully"

  when defined(gtk4):
    # GTK4: run a simple loop, keep the window alive
    echo "Entering GTK4 main loop (simple keep-alive)..."
    while true:
      sleep(100)
  else:
    gtkMain()

  debug("GTK main loop exited")

proc main() =
  ## Main entry point

  # Parse command line arguments
  let opts = parseCliOptions(commandLineParams(), AppName, AppDesc)

  # Handle help and version first
  if opts.positionalArgs.len == 1:
    case opts.positionalArgs[0]:
    of "--help", "-h":
      showUsage(AppName, AppDesc)
      quit(0)
    of "--version", "-v":
      echo "Nemo IDE ", VERSION
      quit(0)

  # Configure logging
  setupLogging(opts.logLevel)

  # Run the IDE
  runIde(opts)

# Entry point
when isMainModule:
  main()
