import std/strformat
import ../compiler/context
import ../compiler/symbols
import ../compiler/types as compTypes

# ============================================================================
# Slot Access Code Generation
# Generates fast O(1) slot accessors
# ============================================================================

proc genSlotConsts*(proto: PrototypeInfo): string =
  ## Generate slot constant declarations
  result = ""

  if proto.slots.len == 0:
    return

  result.add("  # Slot constants\n")
  for i, slot in proto.slots:
    result.add(fmt("  const {manglePrototype(proto.name)}_{mangleSlot(slot.name)}_index = {i}\n"))
  result.add("\n")

proc genSlotGetter*(proto: PrototypeInfo, slot: SlotDef): string =
  ## Generate optimized slot getter procedure
  let protoName = manglePrototype(proto.name)
  let slotName = mangleSlot(slot.name)
  let nimType = compTypes.toNimType(slot.constraint)

  var output = ""
  output.add(fmt("proc nt_{protoName}_{slotName}*(self: ref ProtoObject): NodeValue {{.cdecl, exportc, inline.}} =\n"))
  output.add(fmt("  ## Get slot '{slot.name}' from {proto.name}\n"))
  output.add(fmt("  if self == nil or self.slots.len <= {slot.index}:\n"))
  output.add("    return NodeValue(kind: vkNil)\n")
  output.add(fmt("  return self.slots[{slot.index}]\n\n"))
  return output

proc genSlotSetter*(proto: PrototypeInfo, slot: SlotDef): string =
  ## Generate optimized slot setter procedure
  let protoName = manglePrototype(proto.name)
  let slotName = mangleSlot(slot.name)

  var output = ""
  output.add(fmt("proc nt_{protoName}_{slotName}_put*(self: ref ProtoObject, value: NodeValue): NodeValue {{.cdecl, exportc.}} =\n"))
  output.add(fmt("  ## Set slot '{slot.name}' on {proto.name}\n"))
  output.add("  if self == nil:\n")
  output.add("    return NodeValue(kind: vkNil)\n")
  output.add(fmt("  while self.slots.len <= {slot.index}:\n"))
  output.add("    self.slots.add(NodeValue(kind: vkNil))\n")
  output.add(fmt("  self.slots[{slot.index}] = value\n"))
  output.add("  return value\n\n")
  return output

proc genSlotAccessors*(proto: PrototypeInfo): string =
  ## Generate all slot accessors for a prototype
  result = ""

  if proto.slots.len == 0:
    return

  result.add("# Slot accessors\n")
  result.add("################################\n\n")

  for slot in proto.slots:
    if not slot.isInherited:
      result.add(genSlotGetter(proto, slot))
      result.add(genSlotSetter(proto, slot))

  return result

proc genSlotIndexLookup*(proto: PrototypeInfo): string =
  ## Generate slot index lookup table for dynamic access
  var slots = proto.getAllSlots()

  var output = ""
  output.add(fmt("const {manglePrototype(proto.name)}_slotNames*: array[{slots.len}, string] = [\n"))
  for i, slot in slots:
    output.add(fmt("  \"{slot.name}\""))
    if i < slots.len - 1:
      output.add(",")
    output.add("\n")

  output.add("]\n\n")
  return output

proc genSlotIndexConsts*(proto: PrototypeInfo): string =
  ## Generate constants for all slot indices
  var slots = proto.getAllSlots()

  var output = ""
  output.add(fmt("# Slot indices for {proto.name}\n"))
  for i, slot in slots:
    output.add(fmt("const {manglePrototype(proto.name)}_slot_{mangleSlot(slot.name)} = {i}\n"))
  output.add("\n")

  return output

proc genSlotAccessExpression*(proto: PrototypeInfo, slotName: string,
                              isRead: bool = true): string =
  ## Generate inline slot access expression
  let idx = proto.getSlotIndex(slotName)

  if idx < 0:
    return if isRead: "NodeValue(kind: vkNil)" else: "NodeValue(kind: vkNil)"

  if isRead:
    return fmt("self.slots[{idx}]")
  else:
    return fmt("(proc(val: NodeValue): NodeValue = self.slots[{idx}] = val; val)(value)")
