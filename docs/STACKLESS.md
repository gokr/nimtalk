# Stackless VM Design

## Overview

The Nemo VM (`src/nemo/interpreter/vm.nim`) implements an iterative AST interpreter using an explicit work queue instead of recursive Nim procedure calls. This enables:

1. **True cooperative multitasking** - yield within statements
2. **Stack reification** - `thisContext` accessible from Nemo
3. **No Nim stack overflow** - on deep recursion
4. **Easier debugging and profiling** - flat execution loop

## Architecture

| Aspect | Old Evaluator (evalOld) | Stackless VM |
|--------|------------------------|--------------|
| Lines of code | ~3,260 | ~1,200 |
| Execution model | Recursive Nim calls | Explicit work queue |
| Stack depth | Nim call stack | User-managed work queue |
| Entry point | `evalOld()` | `evalWithVM()`, `doitStackless()` |

## Why the VM is Smaller

### 1. Unified Execution Loop

The recursive evaluator has separate code paths for every operation that needs to suspend/continue. The VM collapses this into a single `while interp.hasWorkFrames()` loop:

```nim
# VM execution loop (single, flat)
while interp.hasWorkFrames():
  let frame = interp.popWorkFrame()
  case frame.kind
  of wfEvalNode: handleEvalNode(...)
  of wfSendMessage: handleContinuation(...)
  # ... all operations handled uniformly
```

### 2. Continuation-Passing Style

Instead of nested function calls, the VM pushes continuation frames onto the work queue:

```nim
# Old: recursive evaluation
let receiver = eval(msg.receiver)
let args = evalArguments(msg.arguments)
return sendMessage(receiver, selector, args)

# New: push continuation frames
interp.pushWorkFrame(wfAfterReceiver(selector, args))
interp.pushWorkFrame(wfEvalNode(msg.receiver))
# ... later handles args, then send
```

## Implementation Status

All features are fully implemented:

| Feature | Status | Notes |
|---------|--------|-------|
| Literals (int, float, string, bool, nil) | Implemented | Direct push to eval stack |
| Identifiers | Implemented | Variable lookup via `lookupVariable` |
| Pseudo-variables (self, nil, true, false, super) | Implemented | With class method support |
| Message sends | Implemented | Full dispatch chain including class methods |
| Block literals | Implemented | With environment capture |
| Block invocation (value, value:, etc.) | Implemented | Via `wfApplyBlock` |
| Variable assignment | Implemented | Via `=` continuation |
| Slot access (O(1) by index) | Implemented | Read and write |
| Array literals | Implemented | Empty and with elements |
| Table literals with entries | Implemented | Key-value pair evaluation |
| Object literals | Implemented | Property initialization |
| Native methods | Implemented | Both with and without interpreter param |
| Interpreted methods | Implemented | Full activation management |
| Return statements | Implemented | Local and non-local returns |
| Cascade messages (`;`) | Implemented | Via `wfCascade` frames |
| Super sends | Implemented | Qualified and unqualified |
| Primitive nodes | Implemented | Fallback statement evaluation |
| Primitive calls | Implemented | Via message send mechanism |
| Cooperative multitasking | Implemented | Yield and resume support |

## Key Data Structures

### WorkFrame

Represents a unit of work to be executed. Kinds include:

- `wfEvalNode` - Evaluate an AST node
- `wfSendMessage` - Send message with args on stack
- `wfAfterReceiver` - After receiver eval, evaluate args
- `wfAfterArg` - After arg N eval, continue to arg N+1 or send
- `wfApplyBlock` - Apply block with captured environment
- `wfPopActivation` - Pop activation and restore state
- `wfReturnValue` - Handle return statement
- `wfBuildArray` - Build array from N values on stack
- `wfBuildTable` - Build table from key-value pairs on stack
- `wfCascade` - Cascade messages to same receiver
- `wfCascadeMessage` - Send one message in a cascade
- `wfCascadeMessageDiscard` - Send message and discard result
- `wfRestoreReceiver` - Restore receiver after cascade

### Interpreter State (VM-specific fields)

```nim
workQueue*: seq[WorkFrame]    # What to do next
evalStack*: seq[NodeValue]    # Values produced
shouldYield*: bool            # Yield flag for cooperative multitasking
```

## Execution Example

Evaluating `3 + 4`:

```
Initial workQueue: [wfEvalNode(MessageNode(receiver=3, selector="+", args=[4]))]

Step 1: Pop wfEvalNode(Message)
        - Recognizes message send
        - Push wfAfterReceiver("+", [4])
        - Push wfEvalNode(Literal(3))

Step 2: Pop wfEvalNode(Literal(3))
        - Push 3 to evalStack

Step 3: Pop wfAfterReceiver("+", [4])
        - Receiver (3) is on evalStack
        - Push wfAfterArg("+", [4], index=0)
        - Push wfEvalNode(Literal(4))

Step 4: Pop wfEvalNode(Literal(4))
        - Push 4 to evalStack

Step 5: Pop wfAfterArg("+", [4], index=0)
        - All args evaluated
        - Push wfSendMessage("+", argCount=1)

Step 6: Pop wfSendMessage("+", 1)
        - Pop args: [4]
        - Pop receiver: 3
        - Look up + method on Integer
        - Create activation
        - Push wfPopActivation
        - Push method body statements
```

## Design Strengths

1. **True Stacklessness**: The work queue enables cooperative multitasking—execution can yield at any point and resume later without consuming Nim stack.

2. **Deterministic State**: All execution state is explicit (`workQueue`, `evalStack`, `activationStack`) rather than implicit in Nim's call stack.

3. **Simpler Debugging**: Single-stepping through a flat loop is easier than following deep recursion.

4. **No Stack Overflow**: Deep recursion in Nemo code won't crash the Nim interpreter.

5. **Stack Reification**: The entire Nemo call stack is accessible as data, enabling `thisContext` and debugging introspection.

## Usage

The stackless VM is the default and only evaluator:

```nim
# Evaluate single expression
let result = interp.evalWithVM(node)

# Evaluate source code
let (result, error) = interp.doitStackless(source)

# Evaluate multiple statements
let (results, error) = interp.evalStatementsStackless(source)

# Evaluate for a process (scheduler integration)
let (value, status) = interp.evalForProcess(node)
```

The scheduler uses `evalForProcess` for cooperative multitasking:

```nim
# In scheduler.nim - run one statement in a process
let (evalResult, status) = interp.evalForProcess(stmt)
# status can be: vmCompleted, vmYielded, vmError
```

## VM Status Values

The VM returns a `VMStatus` indicating execution outcome:

- `vmRunning` - Normal execution (internal use)
- `vmYielded` - Processor yielded, can be resumed
- `vmCompleted` - Execution finished
- `vmError` - Error occurred

## Integration with REPL

The REPL (`src/nemo/repl/doit.nim`) uses `doitStackless` exclusively:

```nim
proc doit*(ctx: DoitContext, code: string, dumpAst = false): (NodeValue, string) =
  # Use stackless VM (now the default and only evaluator)
  return ctx.interpreter.doitStackless(code, dumpAst)
```

Interactive evaluation (`src/nemo/repl/interact.nim`) uses `evalWithVM`:

```nim
let result = ctx.interpreter.evalWithVM(node)
```
