import std/[tables]
import ../core/types
import ./context
import ./types

# ============================================================================
# Type Inference
# Infers types for expressions based on constraints and patterns
# ============================================================================

type
  InferenceContext* = ref object
    currentProto*: PrototypeInfo
    slotConstraints*: Table[string, TypeConstraint]
    localTypes*: Table[string, TypeConstraint]

proc newInferenceContext*(proto: PrototypeInfo): InferenceContext =
  ## Create new inference context
  result = InferenceContext(
    currentProto: proto,
    slotConstraints: initTable[string, TypeConstraint](),
    localTypes: initTable[string, TypeConstraint]()
  )

  # Build slot constraints from prototype
  if proto != nil:
    for slot in proto.slots:
      result.slotConstraints[slot.name] = slot.constraint

proc inferNodeType*(ctx: InferenceContext, node: Node): TypeConstraint =
  ## Infer type constraint for a node
  if node == nil:
    return tcNone

  case node.kind
  of nkLiteral:
    let lit = node.LiteralNode
    return constraintForNode(lit.value.kind)

  of nkMessage:
    let msg = node.MessageNode

    # Check for binary operators
    case msg.selector
    of "+", "-", "*", "/":
      # Infer from both operands
      let leftType = ctx.inferNodeType(msg.receiver)
      let rightType = if msg.arguments.len > 0:
                        ctx.inferNodeType(msg.arguments[0])
                      else:
                        tcNone

      if leftType == tcInt and rightType == tcInt:
        return tcInt
      elif leftType == tcFloat or rightType == tcFloat:
        return tcFloat
      return tcNone

    of "<", "<=", ">", ">=", "=", "~=":
      return tcBool

    of "at:":
      # Slot access - look up slot type
      if msg.receiver.LiteralNode != nil and
         msg.receiver.LiteralNode.value.kind == vkSymbol and
         msg.receiver.LiteralNode.value.symVal == "self":
        return tcNone  # Self slot return

      if msg.receiver.LiteralNode != nil:
        let symbol = msg.receiver.LiteralNode.value.symVal
        if symbol in ctx.slotConstraints:
          return ctx.slotConstraints[symbol]

      return tcNone

    of "at:put:":
      # Slot assignment - return type is the assigned value
      if msg.arguments.len >= 2:
        return ctx.inferNodeType(msg.arguments[1])
      return tcNone

    else:
      # Default: assume object type for unknown methods
      return tcNone

  of nkAssign:
    let assign = node.AssignNode
    let exprType = ctx.inferNodeType(assign.expression)
    # Record for local variable inference
    ctx.localTypes[assign.variable] = exprType
    return exprType

  of nkReturn:
    let ret = node.ReturnNode
    if ret.expression != nil:
      return ctx.inferNodeType(ret.expression)
    return tcNone

  of nkBlock:
    return tcBlock

  of nkArray:
    return tcArray

  of nkTable:
    return tcTable

  of nkPrimitive:
    return tcNone  # Primitives have untyped return by default

  else:
    return tcNone

proc inferMethodReturnType*(ctx: InferenceContext, body: seq[Node]): TypeConstraint =
  ## Infer return type from method body (last expression)
  if body.len == 0:
    return tcNone

  var lastExpr: Node = nil
  for node in body:
    if node.kind == nkReturn:
      let retNode = node.ReturnNode
      if retNode.expression != nil:
        lastExpr = retNode.expression
    else:
      lastExpr = node

  if lastExpr != nil:
    return ctx.inferNodeType(lastExpr)
  return tcNone

proc inferParameterTypes*(ctx: InferenceContext, parameters: seq[string],
                          body: seq[Node]): seq[TypeConstraint] =
  ## Infer parameter types from method body usage
  result = newSeq[TypeConstraint](parameters.len)

  # Scan body for parameter usage patterns
  for node in body:
    case node.kind
    of nkMessage:
      let msg = node.MessageNode

      # Check arithmetic operators
      if msg.selector in ["+", "-", "*", "/"]:
        # Check if receiver or argument is a parameter
        for i, param in parameters:
          for arg in msg.arguments:
            if arg.kind == nkLiteral and arg.LiteralNode.value.kind == vkSymbol:
              if arg.LiteralNode.value.symVal == param:
                result[i] = tcInt

      # Check comparison operators
      if msg.selector in ["<", "<=", ">", ">=", "="]:
        for i, param in parameters:
          if msg.receiver.kind == nkLiteral and
             msg.receiver.LiteralNode.value.kind == vkSymbol and
             msg.receiver.LiteralNode.value.symVal == param:
            result[i] = tcInt

          for arg in msg.arguments:
            if arg.kind == nkLiteral and arg.LiteralNode.value.kind == vkSymbol:
              if arg.LiteralNode.value.symVal == param:
                result[i] = tcInt

    else:
      discard

  # Default to tcNone for uncertain parameters
  for i in 0..<result.len:
    if result[i] == tcNone:
      result[i] = tcNone

  return result
