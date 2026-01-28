import std/[strutils, tables]
import ../core/types
import ./context
import ./types

# ============================================================================
# Type Analyzer
# Parses type hints from derive: syntax and builds prototype type info
# ============================================================================

type
  AnalysisResult* = ref object
    prototypes*: Table[string, PrototypeInfo]
    errors*: seq[string]

proc newAnalysisResult*(): AnalysisResult =
  ## Create new analysis result
  result = AnalysisResult(
    prototypes: initTable[string, PrototypeInfo](),
    errors: @[]
  )

proc parseTypeList*(typeList: string): seq[tuple[name: string, constraint: TypeConstraint]] =
  ## Parse type list from derive: #(name: Type age: Int name2:)
  ## Returns (name, constraint) tuples, constraint = tcNone for untyped
  result = @[]

  var remaining = typeList
  remaining = remaining.strip()
  if remaining.startsWith("#(") and remaining.endsWith(")"):
    remaining = remaining[2..^2].strip()

  var pos = 0
  while pos < remaining.len:
    # Skip whitespace
    while pos < remaining.len and remaining[pos].isSpaceAscii():
      inc pos
    if pos >= remaining.len:
      break

    # Extract slot name up to colon
    var nameStart = pos
    while pos < remaining.len and remaining[pos] != ':' and not remaining[pos].isSpaceAscii():
      inc pos
    let slotName = remaining[nameStart..<pos].strip()

    # Skip to colon
    while pos < remaining.len and remaining[pos] != ':':
      inc pos

    if pos >= remaining.len:
      break
    inc pos  # Skip colon

    # Extract type hint after colon
    while pos < remaining.len and remaining[pos].isSpaceAscii():
      inc pos

    var typeStart = pos
    var typeEnd = pos
    while pos < remaining.len and not remaining[pos].isSpaceAscii() and remaining[pos] notin {',', ')'}:
      inc pos
      typeEnd = pos

    let typeHint = if typeStart < typeEnd: remaining[typeStart..<typeEnd].strip() else: ""
    let constraint = if typeHint.len > 0: parseTypeHint(typeHint) else: tcNone

    result.add((slotName, constraint))

    # Skip to next slot or end
    while pos < remaining.len and remaining[pos] notin {',', ')'}:
      inc pos
    if pos < remaining.len and remaining[pos] == ',':
      inc pos

proc extractDeriveChain*(node: Node): (string, string, string) =
  ## Extract (name, parent, typeList) from derive: message
  ## Handles: ClassName := ParentName derive: #(name: Type ...)
  ## Returns ("", "", "") if not a derive: chain
  result = ("", "", "")

  if node.kind != nkMessage:
    return

  let msg = node.MessageNode
  if msg.selector != "at:put:":
    return

  # Check for derive: inside the value
  if msg.arguments.len < 2:
    return

  let receiver = if msg.receiver.kind == nkLiteral:
                   msg.receiver.LiteralNode.value.symVal
                 else:
                   ""

  let deriveMsg = if msg.arguments[1].kind == nkMessage:
                    msg.arguments[1].MessageNode
                  else:
                    nil

  if deriveMsg == nil or deriveMsg.selector != "derive:":
    return

  let parent = if deriveMsg.receiver.kind == nkLiteral and
                  deriveMsg.receiver.LiteralNode.value.kind == vkSymbol:
                  deriveMsg.receiver.LiteralNode.value.symVal
                else:
                  "Object"

  let typeArg = if deriveMsg.arguments.len > 0:
                   deriveMsg.arguments[0]
                 else:
                   nil

  var typeList = ""
  if typeArg != nil and typeArg.kind == nkLiteral:
    let val = typeArg.LiteralNode.value
    if val.kind == vkString:
      typeList = val.strVal
    elif val.kind == vkArray:
      # Handle #(name: Type) array syntax
      # Convert array to type list string
      var parts: seq[string] = @[]
      for elem in typeArg.LiteralNode.value.arrayVal:
        if elem.kind == vkString:
          parts.add(elem.strVal)
      typeList = "#(" & parts.join(" ") & ")"

  return (receiver, parent, typeList)

proc analyzePrototypeDef*(node: Node, ctx: var CompilerContext,
                           parentProto: PrototypeInfo): PrototypeInfo =
  ## Analyze a prototype definition and create PrototypeInfo
  let (protoName, parentName, typeList) = extractDeriveChain(node)

  if protoName.len == 0:
    return nil

  result = newPrototypeInfo(protoName, parentProto)
  ctx.prototypes[protoName] = result

  if typeList.len > 0:
    let slots = parseTypeList(typeList)
    for (name, constraint) in slots:
      discard result.addSlot(name, constraint)

proc buildPrototypeGraph*(nodes: seq[Node]): AnalysisResult =
  ## Build prototype graph from parsed nodes
  result = newAnalysisResult()
  var ctx = newCompiler()
  var protoMap: Table[string, PrototypeInfo]

  # First pass: collect all prototype declarations
  for node in nodes:
    if node.kind == nkMessage:
      let chain = extractDeriveChain(node)
      let protoName = chain[0]
      if protoName.len > 0 and protoName notin protoMap:
        protoMap[protoName] = nil  # Placeholder

  # Second pass: create ProtoInfo with parent links
  for node in nodes:
    if node.kind == nkMessage:
      let chain = extractDeriveChain(node)
      let protoName = chain[0]
      let parentName = chain[1]
      let typeList = chain[2]

      if protoName.len > 0:
        let parent = if parentName in protoMap: protoMap[parentName] else: nil

        if protoName in ctx.prototypes:
          result.errors.add("Duplicate prototype definition: " & protoName)
          continue

        let proto = newPrototypeInfo(protoName, parent)
        ctx.prototypes[protoName] = proto

        if typeList.len > 0:
          let slots = parseTypeList(typeList)
          for (name, constraint) in slots:
            discard proto.addSlot(name, constraint)

  result.prototypes = ctx.prototypes

  # Resolve slot indices
  for proto in ctx.prototypes.mvalues:
    proto.slotIndex.clear()
    var idx = proto.parent.getAllSlots().len
    for slot in proto.slots.mitems:
      if not slot.isInherited:
        slot.index = idx
        proto.slotIndex[slot.name] = idx
        inc idx

  return result
