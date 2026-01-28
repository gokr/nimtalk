import std/[strutils, sequtils, strformat]
import ../core/types
import ../compiler/context
import ../compiler/symbols

# ============================================================================
# Expression Code Generation
# Generates Nim code from AST expressions
# ============================================================================

type
  GenContext* = ref object
    proto*: PrototypeInfo
    inBlock*: bool

# Forward declaration
proc genExpression*(ctx: GenContext, node: Node): string

proc newGenContext*(proto: PrototypeInfo): GenContext =
  ## Create new generation context
  result = GenContext(
    proto: proto,
    inBlock: false
  )

proc genLiteral*(node: LiteralNode): string =
  ## Generate code for literal node
  let value = node.value
  case value.kind
  of vkInt:
    return fmt("NodeValue(kind: vkInt, intVal: {value.intVal})")
  of vkFloat:
    return fmt("NodeValue(kind: vkFloat, floatVal: {value.floatVal})")
  of vkString:
    return fmt("NodeValue(kind: vkString, strVal: \"{value.strVal}\")")
  of vkSymbol:
    # Symbols need runtime lookup
    return fmt("NodeValue(kind: vkSymbol, symVal: \"{value.symVal}\")")
  of vkBool:
    return fmt("NodeValue(kind: vkBool, boolVal: {value.boolVal})")
  of vkNil:
    return "NodeValue(kind: vkNil)"
  of vkObject:
    return "self.toValue()"
  else:
    return "NodeValue(kind: vkNil)"

proc genSymbolAccess*(ctx: GenContext, name: string): string =
  ## Generate code for symbol/variable access
  if name == "self":
    return "self.toValue()"

  # Check if it's a slot
  if ctx.proto != nil:
    let slotIdx = ctx.proto.getSlotIndex(name)
    if slotIdx >= 0:
      return fmt("self.slots[{slotIdx}]")

  # Fallback: treat as symbol
  return fmt("NodeValue(kind: vkSymbol, symVal: \"{name}\")")

proc genMessage*(ctx: GenContext, node: MessageNode): string =
  ## Generate code for message send
  var output = ""

  # Generate receiver
  var receiverCode: string
  if node.receiver != nil:
    receiverCode = genExpression(ctx, node.receiver)
  else:
    receiverCode = "self.toValue()"

  case node.selector
  of "+", "-", "*", "/":
    # Binary operators
    if node.arguments.len >= 1:
      let argCode = genExpression(ctx, node.arguments[0])
      return fmt("{mangleSelector(node.selector)}({receiverCode}, {argCode})")
    return receiverCode

  of "<", "<=", ">", ">=", "=":
    # Comparison operators
    if node.arguments.len >= 1:
      let argCode = genExpression(ctx, node.arguments[0])
      return fmt("{mangleSelector(node.selector)}({receiverCode}, {argCode})")
    return receiverCode

  of "at:":
    # Slot access
    if node.arguments.len >= 1:
      let argCode = genExpression(ctx, node.arguments[0])
      # Check if accessing self slot
      if node.receiver != nil and node.receiver.kind == nkLiteral:
        let lit = node.receiver.LiteralNode
        if lit.value.kind == vkSymbol and lit.value.symVal == "self":
          return fmt("self.slots[0]")  # Placeholder - use actual slot lookup
      return fmt("sendMessage(runtime, {receiverCode}, \"at:\", @[{argCode}])")
    return receiverCode

  of "at:put:":
    # Slot assignment
    if node.arguments.len >= 2:
      let keyCode = genExpression(ctx, node.arguments[0])
      let valCode = genExpression(ctx, node.arguments[1])
      return fmt("sendMessage(runtime, {receiverCode}, \"at:put:\", @[{keyCode}, {valCode}])")
    return receiverCode

  else:
    # Generic message
    let args = node.arguments.mapIt(genExpression(ctx, it)).join(", ")
    return fmt("sendMessage(runtime, {receiverCode}, \"{node.selector}\", @[{args}])")

proc genExpression*(ctx: GenContext, node: Node): string =
  ## Dispatch to appropriate expression generator

  if node == nil:
    return "NodeValue(kind: vkNil)"

  case node.kind
  of nkLiteral:
    return genLiteral(node.LiteralNode)

  of nkMessage:
    return genMessage(ctx, node.MessageNode)

  of nkAssign:
    let assign = node.AssignNode
    let varName = assign.variable
    let exprCode = genExpression(ctx, assign.expression)

    # Check if variable is a slot
    if ctx.proto != nil and ctx.proto.getSlotIndex(varName) >= 0:
      let idx = ctx.proto.getSlotIndex(varName)
      return fmt("(proc(): NodeValue = self.slots[{idx}] = {exprCode})()")

    # Local variable assignment
    return fmt("var {varName} = {exprCode}")

  of nkReturn:
    let ret = node.ReturnNode
    if ret.expression != nil:
      return "return " & genExpression(ctx, ret.expression)
    return "return self.toValue()"

  of nkArray:
    let arr = node.ArrayNode
    let elems = arr.elements.mapIt(genExpression(ctx, it)).join(", ")
    return fmt("NodeValue(kind: vkArray, arrayVal: @[{elems}])")

  of nkTable:
    let tbl = node.TableNode
    var entries: seq[string] = @[]
    for (key, val) in tbl.entries:
      let keyCode = genExpression(ctx, key)
      let valCode = genExpression(ctx, val)
      entries.add(fmt("{keyCode}: {valCode}"))
    return fmt("NodeValue(kind: vkTable, tableVal: {{{entries.join(\", \")}}})")

  of nkBlock:
    let blk = node.BlockNode
    if blk.parameters.len > 0:
      # Block with parameters
      let params = blk.parameters.join(", ")
      var bodyStmts: seq[string] = @[]
      for stmt in blk.body:
        bodyStmts.add(genExpression(ctx, stmt))
      return fmt("(proc({params}): NodeValue = {bodyStmts.join(\"; \")})")
    else:
      # Simple block
      var bodyStmts: seq[string] = @[]
      for stmt in blk.body:
        bodyStmts.add(genExpression(ctx, stmt))
      return "(proc(): NodeValue = " & bodyStmts.join("; ") & ")"

  of nkPrimitive:
    # Primitives are handled separately
    return "NodeValue(kind: vkNil)"

  else:
    return "NodeValue(kind: vkNil)"

proc genBlockBody*(ctx: GenContext, blkNode: BlockNode): string =
  ## Generate code for block body (sequence of statements)
  var output = ""

  for stmt in blkNode.body:
    let stmtCode = genExpression(ctx, stmt)
    if stmt.kind == nkReturn:
      output.add("  " & stmtCode & "\n")
    elif stmt.kind == nkAssign:
      output.add("  " & stmtCode & "\n")
    else:
      output.add("  result = " & stmtCode & "\n")

  return output

proc genTemporaries*(tmp: seq[string]): string =
  ## Generate temporary variable declarations
  if tmp.len == 0:
    return ""

  var output = "  # Temporaries\n"
  for t in tmp:
    output.add(fmt("  var {t} = NodeValue(kind: vkNil)\n"))
  output.add("\n")
  return output

proc genParameters*(params: seq[string]): string =
  ## Generate parameter declarations for method signature
  if params.len == 0:
    return ""

  var parts: seq[string] = @[]
  for p in params:
    parts.add(fmt("{p}: NodeValue"))
  return parts.join(", ")
