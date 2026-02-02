import std/[tables, sequtils, strformat]
import ../core/types
import ../interpreter/[objects, activation]

# ============================================================================
# Nemo Runtime
# Runtime support for compiled Nemo code
# ============================================================================

type
  Runtime* = ref object
    rootObject*: RuntimeObject
    classes*: Table[string, RuntimeObject]
    methodCache*: Table[string, CompiledMethod]
    isInitializing*: bool

  CompiledMethod* = ref object
    selector*: string
    arity*: int
    nativeAddr*: pointer
    symbolName*: string

var currentRuntime*: ptr Runtime = nil

proc newRuntime*(): Runtime =
  ## Create new runtime instance
  result = Runtime(
    rootObject: rootObject,
    classes: initTable[string, RuntimeObject](),
    methodCache: initTable[string, CompiledMethod](),
    isInitializing: false
  )

proc initRuntime*() =
  ## Initialize global runtime
  if currentRuntime == nil:
    currentRuntime = cast[ptr Runtime](allocShared(sizeof(Runtime)))
    currentRuntime[] = newRuntime()

proc shutdownRuntime*() =
  ## Shutdown and cleanup runtime
  if currentRuntime != nil:
    # Clean up classes
    for obj in currentRuntime.classes.values:
      obj.methods.clear()
    currentRuntime.classes.clear()
    currentRuntime.methodCache.clear()
    deallocShared(cast[pointer](currentRuntime))
    currentRuntime = nil

proc registerClass*(runtime: var Runtime, name: string, cls: RuntimeObject) =
  ## Register a class in the runtime
  runtime.classes[name] = cls
  cls.tags.add(name)

proc getClass*(runtime: Runtime, name: string): RuntimeObject =
  ## Get a registered class by name
  if name in runtime.classes:
    return runtime.classes[name]
  return nil

proc registerMethod*(runtime: var Runtime, selector: string,
                     nativeAddr: pointer, arity: int = 0,
                     symbolName: string = ""): void =
  ## Register a compiled method
  let meth = CompiledMethod(
    selector: selector,
    arity: arity,
    nativeAddr: nativeAddr,
    symbolName: if symbolName.len > 0: symbolName else: selector
  )
  runtime.methodCache[selector] = meth

proc evalBlock*(runtime: Runtime, blk: BlockNode,
                args: seq[NodeValue] = @[]): NodeValue =
  ## Evaluate a block (placeholder - needs full evaluator integration)
  discard
  return NodeValue(kind: vkNil)

proc sendMessage*(runtime: Runtime, receiver: NodeValue,
                  selector: string, args: seq[NodeValue]): NodeValue =
  ## Send a message to a receiver (dynamic dispatch)
  ## This is the slow path fallback for compiled code
  discard
  return NodeValue(kind: vkNil)

# Convenience procs for common operations

proc toValue*(obj: RuntimeObject): NodeValue =
  ## Convert RuntimeObject to NodeValue
  if obj == nil:
    return NodeValue(kind: vkNil)
  return NodeValue(kind: vkObject, objVal: obj)

proc toNodeValue*(obj: RuntimeObject): NodeValue =
  ## Alias for toValue
  return obj.toValue()

proc toInt*(value: NodeValue): int =
  ## Get integer value, raise error if not an integer
  if value.kind != vkInt:
    raise newException(ValueError, fmt("Expected Int, got {value.kind}"))
  return value.intVal

proc toFloat*(value: NodeValue): float64 =
  ## Get float value, raise error if not a float
  if value.kind == vkFloat:
    return value.floatVal
  if value.kind == vkInt:
    return float(value.intVal)
  raise newException(ValueError, fmt("Expected Float, got {value.kind}"))

proc toString*(value: NodeValue): string =
  ## Get string value, raise error if not a string
  if value.kind != vkString:
    raise newException(ValueError, fmt("Expected String, got {value.kind}"))
  return value.strVal

proc toBool*(value: NodeValue): bool =
  ## Get boolean value, raise error if not a boolean
  if value.kind != vkBool:
    raise newException(ValueError, fmt("Expected Bool, got {value.kind}"))
  return value.boolVal

# Slot access helpers

proc getSlot*(obj: RuntimeObject, name: string): NodeValue =
  ## Get slot value by name (O(1) if slot exists)
  if obj == nil or not obj.hasSlots:
    return NodeValue(kind: vkNil)

  if name in obj.slotNames:
    let idx = obj.slotNames[name]
    if idx < obj.slots.len:
      return obj.slots[idx]

  return NodeValue(kind: vkNil)

proc setSlot*(obj: RuntimeObject, name: string, value: NodeValue): NodeValue =
  ## Set slot value by name
  if obj == nil:
    return NodeValue(kind: vkNil)

  if not obj.hasSlots:
    obj.hasSlots = true
    obj.slotNames = initTable[string, int]()

  if name notin obj.slotNames:
    obj.slotNames[name] = obj.slots.len
    obj.slots.add(value)
  else:
    let idx = obj.slotNames[name]
    while obj.slots.len <= idx:
      obj.slots.add(NodeValue(kind: vkNil))
    obj.slots[idx] = value

  return value
