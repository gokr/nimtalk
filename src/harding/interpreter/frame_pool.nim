## Frame Pool for WorkFrame Recycling
## Reduces GC pressure by reusing WorkFrame objects

import std/logging
import ../core/types

const
  InitialPoolSize = 256  # Initial number of frames to pre-allocate
  MaxPoolSize = 1024     # Maximum frames to keep in pool

type
  WorkFramePool* = object
    ## Pool of reusable WorkFrame objects
    ## When a frame is needed, we acquire from pool (or allocate if empty)
    ## When done, we release back to pool (up to MaxPoolSize)
    frames: seq[WorkFrame]  # Available frames in pool
    acquiredCount: int      # Number of frames currently in use (for debugging)
    totalAllocations: int   # Total allocations since start (for stats)

var globalFramePool: WorkFramePool  # Global pool instance

proc initFramePool*() =
  ## Initialize the global frame pool with pre-allocated frames
  globalFramePool.frames = newSeq[WorkFrame](InitialPoolSize)
  for i in 0..<InitialPoolSize:
    globalFramePool.frames[i] = WorkFrame()
  globalFramePool.acquiredCount = 0
  globalFramePool.totalAllocations = InitialPoolSize

proc clearFrame*(frame: WorkFrame) =
  ## Clear all fields of a frame
  if frame == nil:
    return
  frame.node = nil
  frame.selector = ""
  frame.argCount = 0
  frame.msgNode = nil
  frame.isClassMethod = false
  frame.blockVal = nil
  frame.blockArgs = @[]
  frame.pendingSelector = ""
  frame.pendingArgs = @[]
  frame.currentArgIndex = 0
  frame.returnValue = nilValue()
  frame.cascadeMessages = @[]
  frame.cascadeReceiver = nilValue()
  frame.savedReceiver = nil
  frame.isBlockActivation = false
  frame.savedEvalStackDepth = 0
  frame.conditionResult = false
  frame.thenBlock = nil
  frame.elseBlock = nil
  frame.loopKind = false
  frame.conditionBlock = nil
  frame.bodyBlock = nil
  frame.loopState = lsEvaluateCondition
  frame.exceptionClass = nil
  frame.handlerBlock = nil
  frame.exceptionInstance = nil

proc acquireFrame*(): WorkFrame =
  ## Acquire a frame from the pool, or allocate new if pool is empty
  if globalFramePool.frames.len > 0:
    result = globalFramePool.frames.pop()
    # Clear stale data from previous use
    clearFrame(result)
    globalFramePool.acquiredCount += 1
  else:
    # Pool is empty, allocate new frame
    result = WorkFrame()
    globalFramePool.totalAllocations += 1

proc releaseFrame*(frame: WorkFrame) =
  ## Release a frame back to the pool for reuse
  if frame == nil:
    return

  # Note: We don't clear fields here because the frame is passed by reference
  # to handler functions and may still be accessed after release.
  # The acquireFrame function will clear fields when the frame is reused.

  # Return to pool if not at max size
  if globalFramePool.frames.len < MaxPoolSize:
    globalFramePool.frames.add(frame)

  globalFramePool.acquiredCount -= 1

proc getFramePoolStats*(): tuple[available: int, inUse: int, totalAllocated: int] =
  ## Get current frame pool statistics
  result = (
    available: globalFramePool.frames.len,
    inUse: globalFramePool.acquiredCount,
    totalAllocated: globalFramePool.totalAllocations
  )

# Initialize pool on module load
initFramePool()
