import std/[tables, logging]
import ../core/types
import ../core/process
import ../interpreter/evaluator
import ../interpreter/objects
import ../interpreter/activation

# ============================================================================
# Scheduler Integration with Interpreter
# This module connects the green threads scheduler with the Nemo interpreter
# ============================================================================

type
  SchedulerContext* = ref object
    ## Full scheduler context with interpreter integration
    scheduler*: Scheduler
    mainProcess*: Process  ## The initial/main process

# ============================================================================
# Scheduler Context Creation
# ============================================================================

proc newSchedulerContext*(): SchedulerContext =
  ## Create a new scheduler context with a main interpreter
  result = SchedulerContext()

  # Create main interpreter
  var mainInterp = newInterpreter()
  initGlobals(mainInterp)

  # Create scheduler with shared globals and rootObject
  result.scheduler = newScheduler(
    globals = mainInterp.globals,
    root = mainInterp.rootObject
  )

  # Create main process
  result.mainProcess = result.scheduler.newProcess("main")
  result.mainProcess.interpreter = cast[InterpreterRef](mainInterp)
  result.scheduler.addProcess(result.mainProcess)

proc getInterpreter*(process: Process): Interpreter =
  ## Get the interpreter for a process
  ## Note: Interpreter is already a ref object, so we just cast back
  cast[Interpreter](process.interpreter)

proc setInterpreter*(process: Process, interp: Interpreter) =
  ## Set the interpreter for a process
  ## Note: Interpreter is a ref object, store it as pointer to avoid circular import
  process.interpreter = cast[InterpreterRef](interp)

# ============================================================================
# Process Forking
# ============================================================================

proc forkProcess*(ctx: SchedulerContext, blockNode: BlockNode,
                  receiver: Instance, name: string = ""): Process =
  ## Create a new green process from a Nemo block
  ## The new process will execute the block when scheduled

  let sched = ctx.scheduler

  # Create new interpreter sharing globals and rootObject
  let newInterp = newInterpreterWithShared(
    sched.sharedGlobals,
    sched.rootObject
  )

  # Create the process
  result = sched.newProcess(name)
  result.setInterpreter(newInterp)

  # Set up initial activation frame for the block
  let activation = newActivation(blockNode, receiver, nil)
  newInterp.activationStack.add(activation)
  newInterp.currentActivation = activation
  newInterp.currentReceiver = receiver

  # Add to scheduler
  sched.addProcess(result)

  debug("Forked new process: ", result.name, " (pid ", result.pid, ")")

# ============================================================================
# Execution Control
# ============================================================================

proc runCurrentProcess*(ctx: SchedulerContext): NodeValue =
  ## Run one evaluation step on the current process
  ## Returns the result of the last evaluated expression
  let sched = ctx.scheduler
  if sched.currentProcess == nil:
    return nilValue()

  var interp = sched.currentProcess.getInterpreter()
  let activation = interp.currentActivation

  if activation == nil or activation.currentMethod == nil:
    # Process has finished
    sched.terminateProcess(sched.currentProcess)
    return nilValue()

  # Check if process has already returned
  if activation.hasReturned:
    sched.terminateProcess(sched.currentProcess)
    return activation.returnValue

  # Get next statement to execute
  let body = activation.currentMethod.body
  if activation.pc >= body.len:
    # Finished all statements
    sched.terminateProcess(sched.currentProcess)
    return interp.lastResult

  # Execute one statement
  let stmt = body[activation.pc]
  inc activation.pc

  try:
    result = interp.eval(stmt)
    interp.lastResult = result
  except EvalError as e:
    # Process encountered an error - terminate it
    debug("Process ", sched.currentProcess.name, " error: ", e.msg)
    sched.terminateProcess(sched.currentProcess)
    return nilValue()

  # Check if block finished
  if activation.pc >= body.len or activation.hasReturned:
    sched.terminateProcess(sched.currentProcess)

proc runOneSlice*(ctx: SchedulerContext): bool =
  ## Run one complete time slice (one statement from one process)
  ## Returns true if something was executed, false if no ready processes
  let sched = ctx.scheduler

  if not sched.hasReadyProcesses():
    return false

  # Select next process
  let process = sched.selectNextProcess()
  if process == nil:
    return false

  # Run one step
  discard ctx.runCurrentProcess()

  # If process is still running (not terminated/blocked), yield it
  if process.state == psRunning:
    sched.yieldCurrentProcess()

  return true

proc runUntilIdle*(ctx: SchedulerContext, maxSteps: int = 10000): int =
  ## Run processes until none are ready or maxSteps reached
  ## Returns number of steps executed
  result = 0
  while result < maxSteps and ctx.runOneSlice():
    inc result

proc runToCompletion*(ctx: SchedulerContext, maxSteps: int = 100000): int =
  ## Run all processes until all are terminated or maxSteps reached
  ## Returns number of steps executed
  result = 0
  let sched = ctx.scheduler
  while result < maxSteps:
    if not ctx.runOneSlice():
      # No ready processes - check if any are blocked
      if sched.blockedCount == 0:
        break  # All done
      # Some are blocked - would need external event to unblock
      break
    inc result

# ============================================================================
# Nemo-side Processor Object
# ============================================================================

# Processor yield implementation
proc processorYieldImpl(interp: var Interpreter, self: Instance,
                        args: seq[NodeValue]): NodeValue =
  ## Processor yield - yields the current process
  ## This is called from Nemo code and triggers a context switch
  ## The actual yield happens in the scheduler loop, so we just return nil
  ## and the scheduler will handle the yield after this method returns
  debug("Processor yield called")
  return nilValue()

# Processor fork: implementation (placeholder - needs scheduler context)
proc processorForkImpl(interp: var Interpreter, self: Instance,
                       args: seq[NodeValue]): NodeValue =
  ## Processor fork: aBlock - creates a new process to run aBlock
  ## Note: This is a placeholder. Full implementation needs scheduler context.
  if args.len < 1 or args[0].kind != vkBlock:
    return nilValue()

  debug("Processor fork: called with block")
  # For now, just return nil - actual forking needs scheduler integration
  # The full implementation will be in the REPL/runner that has scheduler access
  return nilValue()

# Processor current implementation
proc processorCurrentImpl(interp: var Interpreter, self: Instance,
                          args: seq[NodeValue]): NodeValue =
  ## Processor current - returns the current process (placeholder)
  debug("Processor current called")
  return nilValue()

var processorClass*: Class = nil

proc createProcessorClass*(): Class =
  ## Create the Processor class with yield, fork:, current methods
  if processorClass != nil:
    return processorClass

  # Ensure objectClass is initialized
  discard initCoreClasses()

  processorClass = newClass(parents = @[objectClass], name = "Processor")
  processorClass.tags = @["Processor", "Scheduler"]

  # Add yield method
  let yieldMethod = createCoreMethod("yield")
  yieldMethod.nativeImpl = cast[pointer](processorYieldImpl)
  yieldMethod.hasInterpreterParam = true
  addMethodToClass(processorClass, "yield", yieldMethod)

  # Add fork: method
  let forkMethod = createCoreMethod("fork:")
  forkMethod.nativeImpl = cast[pointer](processorForkImpl)
  forkMethod.hasInterpreterParam = true
  addMethodToClass(processorClass, "fork:", forkMethod)

  # Add current method
  let currentMethod = createCoreMethod("current")
  currentMethod.nativeImpl = cast[pointer](processorCurrentImpl)
  currentMethod.hasInterpreterParam = true
  addMethodToClass(processorClass, "current", currentMethod)

  return processorClass

proc initProcessorGlobal*(interp: var Interpreter) =
  ## Initialize the Processor global in the interpreter
  ## Processor is a singleton instance of ProcessorClass
  let cls = createProcessorClass()
  let processorInstance = newInstance(cls)
  interp.globals[]["Processor"] = processorInstance.toValue()
