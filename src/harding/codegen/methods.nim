import std/strutils
import ../core/types
import ../compiler/context
import ../compiler/symbols

# ============================================================================
# Method Body Code Generation
# Generates type-specialized methods with dynamic fallback
# ============================================================================

proc genMethodSignature*(cls: ClassInfo, meth: BlockNode,
                          selector: string): string =
  ## Generate method signature with properly mangled name
  let nimName = mangleSelector(selector)
  let clsPrefix = if cls != nil: mangleClass(cls.name) & "_" else: ""

  var output = "proc " & clsPrefix & nimName & "*(self: ref RuntimeObject"

  # Add parameters
  for param in meth.parameters:
    output.add(", " & param & ": NodeValue")

  output.add("): NodeValue {.cdecl, exportc.}\n")
  return output

proc genMethodHeader*(cls: ClassInfo, meth: BlockNode,
                      selector: string, className: string = ""): string =
  ## Generate method header with documentation
  let sig = genMethodSignature(cls, meth, selector)
  var output = sig & " =\n"
  output.add("  ## Compiled method: " & selector & "\n")
  output.add("  ## Class: " & className & "\n")
  output.add("  ##\n")
  return output

proc genSlotFastAccess*(cls: ClassInfo, slotName: string): string =
  ## Generate fast path for slot access
  let idx = cls.getSlotIndex(slotName)

  if idx < 0:
    return "  # Dynamic fallback: slot '" & slotName & "' not found in class\n" &
           "  return sendMessage(currentRuntime[], self, \"at:\", @[NodeValue(kind: vkSymbol, symVal: \"" &
           slotName & "\")])\n"

  var output = "\n  var slotValue = NodeValue(kind: vkNil)\n"
  output.add("  if self.slots.len > " & $idx & ":\n")
  output.add("    slotValue = self.slots[" & $idx & "]\n")
  output.add("  return slotValue\n")
  return output

proc genBinaryOpFastPath*(op: string): string =
  ## Generate inline binary operation for numeric types
  var nimOp = ""
  case op
  of "+": nimOp = "+"
  of "-": nimOp = "-"
  of "*": nimOp = "*"
  of "/": nimOp = "/"
  else: nimOp = op

  var output = "\n  # Fast path: both operands are integers\n"
  output.add("  if a.kind == vkInt and b.kind == vkInt:\n")
  if op == "/":
    output.add("    return NodeValue(kind: vkInt, intVal: a.intVal div b.intVal)\n")
  else:
    output.add("    return NodeValue(kind: vkInt, intVal: a.intVal " & nimOp & " b.intVal)\n")
  output.add("  # Fast path: both operands are floats\n")
  output.add("  if a.kind == vkFloat and b.kind == vkFloat:\n")
  output.add("    return NodeValue(kind: vkFloat, floatVal: a.floatVal " & nimOp & " b.floatVal)\n")
  output.add("  # Mixed int/float - promote to float\n")
  output.add("  if a.kind == vkInt and b.kind == vkFloat:\n")
  output.add("    return NodeValue(kind: vkFloat, floatVal: float(a.intVal) " & nimOp & " b.floatVal)\n")
  output.add("  if a.kind == vkFloat and b.kind == vkInt:\n")
  output.add("    return NodeValue(kind: vkFloat, floatVal: a.floatVal " & nimOp & " float(b.intVal))\n")
  return output

proc genComparisonFastPath*(op: string): string =
  ## Generate inline comparison operation
  var nimOp = ""
  case op
  of "<": nimOp = "<"
  of "<=": nimOp = "<="
  of ">": nimOp = ">"
  of ">=": nimOp = ">="
  of "=": nimOp = "=="
  of "~=": nimOp = "!="
  else: nimOp = "!="

  var output = "\n  # Fast path: integer comparison\n"
  output.add("  if a.kind == vkInt and b.kind == vkInt:\n")
  output.add("    return NodeValue(kind: vkBool, boolVal: a.intVal " & nimOp & " b.intVal)\n")
  output.add("  # Fast path: float comparison\n")
  output.add("  if a.kind == vkFloat and b.kind == vkFloat:\n")
  output.add("    return NodeValue(kind: vkBool, boolVal: a.floatVal " & nimOp & " b.floatVal)\n")
  output.add("  # Fast path: string comparison\n")
  output.add("  if a.kind == vkString and b.kind == vkString:\n")
  output.add("    return NodeValue(kind: vkBool, boolVal: a.strVal " & nimOp & " b.strVal)\n")
  output.add("  # Fast path: bool comparison\n")
  output.add("  if a.kind == vkBool and b.kind == vkBool:\n")
  output.add("    return NodeValue(kind: vkBool, boolVal: a.boolVal " & nimOp & " b.boolVal)\n")
  return output

proc genMethodBody*(cls: ClassInfo, meth: BlockNode,
                     selector: string): string =
  ## Generate method body with type specialization and fallback

  # Check for known patterns
  case selector
  of "+", "-", "*", "/":
    return "\n  # Arithmetic operator not yet implemented with type specialization\n" &
           "  return sendMessage(currentRuntime[], self, \"" & selector & "\", @[a])\n"

  of "<", "<=", ">", ">=", "=":
    return "\n  # Comparison operator not yet implemented with type specialization\n" &
           "  return sendMessage(currentRuntime[], self, \"" & selector & "\", @[a, b])\n"

  of "at:":
    if meth.parameters.len > 0:
      if cls != nil and meth.parameters[0].len > 0:
        let param = meth.parameters[0]
        return "\n  # Slot access with parameter '" & param & "'\n" &
               "  if param.kind == vkSymbol:\n" &
               "    let slotName = param.symVal\n" &
               "    # Try direct slot access\n" &
               "    if slotName == \"self\":\n" &
               "      return self.toValue()\n" &
               "    # Dynamic dispatch for unknown slots\n" &
               "    return sendMessage(currentRuntime[], self, \"at:\", @[param])\n" &
               "  return NodeValue(kind: vkNil)\n"
    return "\n  return sendMessage(currentRuntime[], self, \"at:\", @[param])\n"

  of "at:put:":
    return "\n  if key.kind == vkSymbol:\n" &
           "    let slotName = key.symVal\n" &
           "    if slotName == \"self\":\n" &
           "      return value\n" &
           "    # Dynamic dispatch for at:put:\n" &
           "    return sendMessage(currentRuntime[], self, \"at:put:\", @[key, value])\n" &
           "  return value\n"

  else:
    # Generic method - use dynamic dispatch
    if meth.parameters.len == 0:
      return "\n  return self.toValue()\n"

    let args = meth.parameters.join(", ")
    return "\n  # Method not yet compiled - using dynamic dispatch\n" &
           "  return sendMessage(currentRuntime[], self, \"" & selector &
           "\", @[" & args & "])\n"

proc genMethod*(cls: ClassInfo, meth: BlockNode,
                selector: string, className: string = ""): string =
  ## Generate complete method procedure
  var output = genMethodHeader(cls, meth, selector, className)
  output.add(genMethodBody(cls, meth, selector))
  output.add("\n\n")
  return output

proc genBinaryOpMethod*(selector: string): string =
  ## Generate a binary operator method (top-level, not class-scoped)
  let nimName = mangleSelector(selector)

  var output = "\nproc " & nimName & "*(a: NodeValue, b: NodeValue): NodeValue {.cdecl, exportc.} =\n"
  output.add("  ## Binary operator: " & selector & "\n")
  output.add("  ## Provides fast-path type specialization with dynamic fallback\n")
  output.add("  ##\n")
  output.add(genBinaryOpFastPath(selector))
  output.add("\n  # Slow path: dynamic dispatch\n")
  output.add("  return sendMessage(currentRuntime[], a, \"" & selector & "\", @[b])\n\n")
  return output

proc genComparisonMethod*(selector: string): string =
  ## Generate a comparison operator method
  let nimName = mangleSelector(selector)

  var output = "\nproc " & nimName & "*(a: NodeValue, b: NodeValue): NodeValue {.cdecl, exportc.} =\n"
  output.add("  ## Comparison operator: " & selector & "\n")
  output.add("  ## Provides fast-path type specialization with dynamic fallback\n")
  output.add("  ##\n")
  output.add(genComparisonFastPath(selector))
  output.add("\n  # Slow path: dynamic dispatch\n")
  output.add("  return sendMessage(currentRuntime[], a, \"" & selector & "\", @[b])\n\n")
  return output

proc genRuntimeHelperMethods*(): string =
  ## Generate runtime helper methods
  result = """
# Runtime helpers for primitives

proc getArg*(args: seq[NodeValue], index: int, default: NodeValue = NodeValue(kind: vkNil)): NodeValue =
  ## Get argument at index with default fallback
  if index >= 0 and index < args.len:
    return args[index]
  return default

proc requireArg*(args: seq[NodeValue], index: int, kind: ValueKind): NodeValue =
  ## Get argument and require specific type
  if index < 0 or index >= args.len:
    raise newException(ValueError, "Missing argument at index " & $index)
  let arg = args[index]
  if arg.kind != kind:
    raise newException(ValueError, "Expected " & $kind & ", got " & $arg.kind)
  return arg

proc intArg*(args: seq[NodeValue], index: int): int =
  ## Get integer argument
  return requireArg(args, index, vkInt).intVal

proc floatArg*(args: seq[NodeValue], index: int): float64 =
  ## Get float argument (converts from int if needed)
  let arg = args[index]
  if arg.kind == vkInt:
    return float(arg.intVal)
  if arg.kind == vkFloat:
    return arg.floatVal
  raise newException(ValueError, "Expected numeric argument at index " & $index)

proc stringArg*(args: seq[NodeValue], index: int): string =
  ## Get string argument
  return requireArg(args, index, vkString).strVal

"""
