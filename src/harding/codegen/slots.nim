import std/strformat
import ../compiler/context
import ../compiler/symbols
import ../compiler/types as compTypes

# ============================================================================
# Slot Access Code Generation
# Generates fast O(1) slot accessors
# ============================================================================

proc genSlotConsts*(cls: ClassInfo): string =
  ## Generate slot constant declarations
  result = ""

  if cls.slots.len == 0:
    return

  result.add("  # Slot constants\n")
  for i, slot in cls.slots:
    result.add(fmt("  const {mangleClass(cls.name)}_{mangleSlot(slot.name)}_index = {i}\n"))
  result.add("\n")

proc genSlotGetter*(cls: ClassInfo, slot: SlotDef): string =
  ## Generate optimized slot getter procedure
  let clsName = mangleClass(cls.name)
  let slotName = mangleSlot(slot.name)
  let nimType = compTypes.toNimType(slot.constraint)

  var output = ""
  output.add(fmt("proc nt_{clsName}_{slotName}*(self: ref RuntimeObject): NodeValue {{.cdecl, exportc, inline.}} =\n"))
  output.add(fmt("  ## Get slot '{slot.name}' from {cls.name}\n"))
  output.add(fmt("  if self == nil or self.slots.len <= {slot.index}:\n"))
  output.add("    return NodeValue(kind: vkNil)\n")
  output.add(fmt("  return self.slots[{slot.index}]\n\n"))
  return output

proc genSlotSetter*(cls: ClassInfo, slot: SlotDef): string =
  ## Generate optimized slot setter procedure
  let clsName = mangleClass(cls.name)
  let slotName = mangleSlot(slot.name)

  var output = ""
  output.add(fmt("proc nt_{clsName}_{slotName}_put*(self: ref RuntimeObject, value: NodeValue): NodeValue {{.cdecl, exportc.}} =\n"))
  output.add(fmt("  ## Set slot '{slot.name}' on {cls.name}\n"))
  output.add("  if self == nil:\n")
  output.add("    return NodeValue(kind: vkNil)\n")
  output.add(fmt("  while self.slots.len <= {slot.index}:\n"))
  output.add("    self.slots.add(NodeValue(kind: vkNil))\n")
  output.add(fmt("  self.slots[{slot.index}] = value\n"))
  output.add("  return value\n\n")
  return output

proc genSlotAccessors*(cls: ClassInfo): string =
  ## Generate all slot accessors for a class
  result = ""

  if cls.slots.len == 0:
    return

  result.add("# Slot accessors\n")
  result.add("################################\n\n")

  for slot in cls.slots:
    if not slot.isInherited:
      result.add(genSlotGetter(cls, slot))
      result.add(genSlotSetter(cls, slot))

  return result

proc genSlotIndexLookup*(cls: ClassInfo): string =
  ## Generate slot index lookup table for dynamic access
  var slots = cls.getAllSlots()

  var output = ""
  output.add(fmt("const {mangleClass(cls.name)}_slotNames*: array[{slots.len}, string] = [\n"))
  for i, slot in slots:
    output.add(fmt("  \"{slot.name}\""))
    if i < slots.len - 1:
      output.add(",")
    output.add("\n")

  output.add("]\n\n")
  return output

proc genSlotIndexConsts*(cls: ClassInfo): string =
  ## Generate constants for all slot indices
  var slots = cls.getAllSlots()

  var output = ""
  output.add(fmt("# Slot indices for {cls.name}\n"))
  for i, slot in slots:
    output.add(fmt("const {mangleClass(cls.name)}_slot_{mangleSlot(slot.name)} = {i}\n"))
  output.add("\n")

  return output

proc genSlotAccessExpression*(cls: ClassInfo, slotName: string,
                              isRead: bool = true): string =
  ## Generate inline slot access expression
  let idx = cls.getSlotIndex(slotName)

  if idx < 0:
    return if isRead: "NodeValue(kind: vkNil)" else: "NodeValue(kind: vkNil)"

  if isRead:
    return fmt("self.slots[{idx}]")
  else:
    return fmt("(proc(val: NodeValue): NodeValue = self.slots[{idx}] = val; val)(value)")
