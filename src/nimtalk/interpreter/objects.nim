import std/[tables, strutils, sequtils]
import ../core/types
import ../parser/parser

# ============================================================================
# Object System for Nimtalk
# Prototype-based objects with delegation
# ============================================================================

# Forward declarations for core method implementations (exported for testing)
proc cloneImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue
proc deriveImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue
proc deriveWithIVarsImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue
proc atImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue
proc atPutImpl*(self: var ProtoObject, args: seq[NodeValue]): NodeValue
proc plusImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue
proc printStringImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue
proc writeImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue
proc writelineImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue
proc doesNotUnderstandImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue

# Global root object (singleton)
var rootObject*: RootObject = nil

# Create a core method
type CoreMethodProc = proc(self: ProtoObject, args: seq[NodeValue]): NodeValue {.nimcall.}

type CoreMutMethodProc = proc(self: var ProtoObject, args: seq[NodeValue]): NodeValue {.nimcall.}

proc createCoreMethod*(name: string): BlockNode =
  ## Create a method stub
  let blk = BlockNode()
  blk.parameters = if ':' in name:
                      name.split(':').filterIt(it.len > 0)
                    else:
                      @[]
  blk.temporaries = @[]
  let placeholder: Node = LiteralNode(value: NodeValue(kind: vkNil))  # Placeholder
  blk.body = @[placeholder]
  blk.isMethod = true
  blk.nativeImpl = nil
  return blk

# Method installation
proc addMethod*(obj: ProtoObject, selector: string, blk: BlockNode) =
  ## Add a method to an object's method dictionary
  obj.methods[selector] = blk

proc addProperty*(obj: ProtoObject, name: string, value: NodeValue) =
  ## Add a property to an object's property dictionary
  obj.properties[name] = value

# Global namespace for storing "classes" and constants
var globals*: Table[string, NodeValue]

# Initialize global namespace
proc initGlobals*() =
  ## Initialize the globals table for storing classes and constants
  if globals.len == 0:
    globals = initTable[string, NodeValue]()

# Add a value to globals (typically a "class")
proc addGlobal*(name: string, value: NodeValue) =
  ## Add a global binding (e.g., Person := Object derive)
  globals[name] = value

# Get a value from globals
proc getGlobal*(name: string): NodeValue =
  ## Get a global binding, return nil if not found
  if globals.hasKey(name):
    return globals[name]
  else:
    return nilValue()

# Check if a global exists
proc hasGlobal*(name: string): bool =
  ## Check if a global binding exists
  return globals.hasKey(name)

# Remove a global
proc removeGlobal*(name: string) =
  ## Remove a global binding
  if globals.hasKey(name):
    globals.del(name)

# Get all global names
proc globalNames*(): seq[string] =
  ## Return all global names
  return toSeq(globals.keys)

# Initialize root object with core methods
proc initRootObject*(): RootObject =
  ## Initialize the global root object with core methods
  if rootObject == nil:
    # Initialize symbol table and globals first
    initSymbolTable()
    initGlobals()

    rootObject = RootObject()
    rootObject.properties = initTable[string, NodeValue]()
    rootObject.methods = initTable[string, BlockNode]()
    rootObject.parents = @[]
    rootObject.tags = @["Object", "Proto"]
    rootObject.isNimProxy = false
    rootObject.nimValue = nil
    rootObject.nimType = ""
    rootObject.hasSlots = false
    rootObject.slots = @[]
    rootObject.slotNames = initTable[string, int]()

    # Add Object to globals immediately
    addGlobal("Object", NodeValue(kind: vkObject, objVal: rootObject))

    # Install core methods
    let cloneMethod = createCoreMethod("clone")
    cloneMethod.nativeImpl = cast[pointer](cloneImpl)
    addMethod(rootObject, "clone", cloneMethod)

    let deriveMethod = createCoreMethod("derive")
    deriveMethod.nativeImpl = cast[pointer](deriveImpl)
    addMethod(rootObject, "derive", deriveMethod)

    let deriveWithIVarsMethod = createCoreMethod("derive:")
    deriveWithIVarsMethod.nativeImpl = cast[pointer](deriveWithIVarsImpl)
    addMethod(rootObject, "derive:", deriveWithIVarsMethod)

    let atMethod = createCoreMethod("at:")
    atMethod.nativeImpl = cast[pointer](atImpl)
    addMethod(rootObject, "at:", atMethod)

    let atPutMethod = createCoreMethod("at:put:")
    atPutMethod.nativeImpl = cast[pointer](atPutImpl)
    addMethod(rootObject, "at:put:", atPutMethod)

    let printStringMethod = createCoreMethod("printString")
    printStringMethod.nativeImpl = cast[pointer](printStringImpl)
    addMethod(rootObject, "printString", printStringMethod)

    let dnuMethod = createCoreMethod("doesNotUnderstand:")
    dnuMethod.nativeImpl = cast[pointer](doesNotUnderstandImpl)
    addMethod(rootObject, "doesNotUnderstand:", dnuMethod)

    # Add arithmetic operators
    let plusMethod = createCoreMethod("+")
    plusMethod.nativeImpl = cast[pointer](plusImpl)
    addMethod(rootObject, "+", plusMethod)

  return rootObject

# Nim-level clone function for ProtoObject - returns NodeValue wrapper
proc clone*(self: ProtoObject): NodeValue =
  ## Shallow clone of ProtoObject (Nim-level clone) wrapped in NodeValue
  let objClone = ProtoObject()
  objClone.properties = self.properties
  objClone.methods = initTable[string, BlockNode]()
  for key, value in self.methods:
    objClone.methods[key] = value
  objClone.parents = self.parents
  objClone.tags = self.tags
  objClone.isNimProxy = self.isNimProxy
  objClone.nimValue = self.nimValue
  objClone.nimType = self.nimType
  objClone.hasSlots = self.hasSlots
  objClone.slots = self.slots  # Copy slots by value (seq is value type)
  objClone.slotNames = self.slotNames
  result = NodeValue(kind: vkObject, objVal: objClone)

# Nim-level clone function for RootObject - returns NodeValue wrapper
proc clone*(self: RootObject): NodeValue =
  ## Shallow clone of RootObject (Nim-level clone) wrapped in NodeValue
  let objClone = RootObject()
  objClone.properties = self.properties
  objClone.methods = initTable[string, BlockNode]()
  for key, value in self.methods:
    objClone.methods[key] = value
  objClone.parents = self.parents
  objClone.tags = self.tags
  objClone.isNimProxy = self.isNimProxy
  objClone.nimValue = self.nimValue
  objClone.nimType = self.nimType
  result = NodeValue(kind: vkObject, objVal: objClone)

# Core method implementations
proc cloneImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Shallow clone of object
  let clone = ProtoObject()
  clone.properties = self.properties
  clone.methods = initTable[string, BlockNode]()
  clone.parents = self.parents
  clone.tags = self.tags
  clone.isNimProxy = false
  clone.nimValue = nil
  clone.nimType = ""
  return NodeValue(kind: vkObject, objVal: clone)

proc deriveImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Create child with self as parent (prototype delegation)
  let child = ProtoObject()
  child.properties = initTable[string, NodeValue]()
  child.methods = initTable[string, BlockNode]()
  child.parents = @[self]
  child.tags = self.tags & @["derived"]
  child.isNimProxy = false
  child.nimValue = nil
  child.nimType = ""
  child.hasSlots = false
  child.slots = @[]
  child.slotNames = initTable[string, int]()
  return NodeValue(kind: vkObject, objVal: child)

proc deriveWithIVarsImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Create child with self as parent and declared instance variables
  ## Also generates accessor methods for all instance variables
  if args.len < 1:
    raise newException(ValueError, "derive:: requires array of ivar names")
  if args[0].kind != vkArray:
    raise newException(ValueError, "derive:: first argument must be array of strings")

  # Extract ivar names from array
  let ivarArray = args[0].arrayVal
  var childIvars: seq[string] = @[]
  for ivarVal in ivarArray:
    if ivarVal.kind != vkSymbol:
      raise newException(ValueError, "derive:: all ivar names must be symbols")
    childIvars.add(ivarVal.symVal)

  # If parent has slots, inherit them first
  var allIvars: seq[string] = @[]
  if self.hasSlots:
    # Inherit parent's ivars
    allIvars = self.getSlotNames()

  # Add child's new ivars (checking for duplicates)
  for ivar in childIvars:
    if ivar in allIvars:
      raise newException(ValueError, "Instance variable conflict: " & ivar & " already defined in parent")
    allIvars.add(ivar)

  # Create object with combined slots
  var child: ProtoObject
  if allIvars.len > 0:
    child = initSlotObject(allIvars)
  else:
    # Fallback to empty object if no ivars
    child = ProtoObject()
    child.properties = initTable[string, NodeValue]()
    child.methods = initTable[string, BlockNode]()
    child.parents = @[]
    child.tags = @[]
    child.isNimProxy = false
    child.nimValue = nil
    child.nimType = ""
    child.hasSlots = false
    child.slots = @[]
    child.slotNames = initTable[string, int]()

  child.parents = @[self]
  child.tags = self.tags & @["derived", "slotted"]

  # Generate accessor methods for all instance variables
  for ivar in allIvars:
    # Generate getter: ivar -> slots[slotNames[ivar]]
    var getterBody: seq[Node] = @[]
    var msgArgs: seq[Node] = @[]
    msgArgs.add(LiteralNode(value: getSymbol(ivar)))
    getterBody.add(ReturnNode(
      expression: MessageNode(
        receiver: nil,  # implicit self
        selector: "getSlot:",
        arguments: msgArgs
      )
    ))

    let getterBlock = BlockNode(
      parameters: @[],
      temporaries: @[],
      body: getterBody,
      isMethod: true
    )
    child.methods[ivar] = getterBlock

    # Generate setter: ivar: value -> slots[slotNames[ivar]] := value
    var setterBody: seq[Node] = @[]
    var setterArgs: seq[Node] = @[]
    setterArgs.add(LiteralNode(value: getSymbol(ivar)))
    setterArgs.add(LiteralNode(value: NodeValue(kind: vkSymbol, symVal: "newValue")))
    setterBody.add(AssignNode(
      variable: "<ignore>",  # Assignment to slot via setSlot
      expression: MessageNode(
        receiver: nil,
        selector: "setSlot:value:",
        arguments: setterArgs
      )
    ))
    setterBody.add(ReturnNode(
      expression: LiteralNode(value: NodeValue(kind: vkSymbol, symVal: "newValue"))
    ))

    let setterBlock = BlockNode(
      parameters: @["newValue"],
      temporaries: @[],
      body: setterBody,
      isMethod: true
    )
    child.methods[ivar & ":"] = setterBlock

  return NodeValue(kind: vkObject, objVal: child)

proc atImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Get property value: obj at: 'key'
  if args.len < 1:
    return nilValue()
  if args[0].kind != vkSymbol:
    return nilValue()

  let key = args[0].symVal
  return getProperty(self, key)

proc atPutImpl*(self: var ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Set property value: obj at: 'key' put: value
  if args.len < 2:
    return nilValue()
  if args[0].kind != vkSymbol:
    return nilValue()

  let key = args[0].symVal
  let value = args[1]
  setProperty(self, key, value)
  return value

proc plusImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Add two numbers: a + b
  if args.len < 1:
    return nilValue()

  let other = args[0]
  if self.isNimProxy and self.nimType == "int" and other.kind == vkInt:
    # Both are integers, add them
    let a = cast[ptr int](self.nimValue)[]
    let b = other.intVal
    return NodeValue(kind: vkInt, intVal: a + b)
  elif other.kind == vkInt:
    # Try to get self as integer from properties
    let selfVal = getProperty(self, "value")
    if selfVal.kind == vkInt:
      return NodeValue(kind: vkInt, intVal: selfVal.intVal + other.intVal)

  return nilValue()

proc printStringImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Default print representation
  if self.isNimProxy:
    return NodeValue(kind: vkString, strVal: "<Nim " & self.nimType & ">")
  elif "Object" in self.tags:
    return NodeValue(kind: vkString, strVal: "<object>")
  else:
    return NodeValue(kind: vkString, strVal: "<unknown>")

proc writeImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Write string to stdout without newline (Stdout write: 'text')
  if args.len < 1:
    return nilValue()
  let strVal = args[0]
  if strVal.kind == vkString:
    stdout.write(strVal.strVal)
    flushFile(stdout)
  return strVal  ## Return the string written

proc writelineImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Write string or integer to stdout with newline (Stdout writeline: value)
  if args.len < 1:
    stdout.write("\n")
  else:
    let value = args[0]
    case value.kind
    of vkString:
      echo value.strVal  ## echo adds newline
    of vkInt:
      echo value.intVal
    else:
      echo value.toString()
  return nilValue()

proc doesNotUnderstandImpl*(self: ProtoObject, args: seq[NodeValue]): NodeValue =
  ## Default handler for unknown messages
  if args.len < 1 or args[0].kind != vkSymbol:
    raise newException(ValueError, "doesNotUnderstand: requires message symbol")

  let selector = args[0].symVal
  raise newException(ValueError, "Message not understood: " & selector)

# Map method names to implementations (not currently used)
# var coreMethods: Table[string, CoreMethodProc]
#
# proc initCoreMethods() =
#   if coreMethods.len == 0:
#     coreMethods = {
#       "clone": cloneImpl,
#       "derive": deriveImpl,
#       "at:": atImpl,
#       "at:put:": atPutImpl,
#       "printString": printStringImpl,
#       "doesNotUnderstand:": doesNotUnderstandImpl
#     }.toTable

# Object creation helpers
proc wrapIntAsObject*(value: int): NodeValue =
  ## Wrap an integer as a Nim proxy object that can receive messages
  let obj = ProtoObject()
  obj.properties = initTable[string, NodeValue]()
  obj.methods = initTable[string, BlockNode]()
  obj.parents = @[rootObject.ProtoObject]
  obj.tags = @["Integer", "Number"]
  obj.isNimProxy = true
  obj.nimValue = cast[pointer](alloc(sizeof(int)))
  cast[ptr int](obj.nimValue)[] = value
  obj.nimType = "int"
  obj.hasSlots = false
  obj.slotNames = initTable[string, int]()
  return NodeValue(kind: vkObject, objVal: obj)

proc newObject*(properties = initTable[string, NodeValue]()): ProtoObject =
  ## Create a new object with optional properties
  let obj = ProtoObject()
  obj.properties = properties
  obj.methods = initTable[string, BlockNode]()
  obj.parents = @[initRootObject().ProtoObject]  # Convert to base type
  obj.tags = @["derived"]
  obj.isNimProxy = false
  obj.nimValue = nil
  obj.nimType = ""
  return obj

# Object comparison
proc isSame*(obj1, obj2: ProtoObject): bool =
  ## Check if two objects are the same (identity)
  return obj1 == obj2

proc inheritsFrom*(obj: ProtoObject, parent: ProtoObject): bool =
  ## Check if object inherits from parent in prototype chain
  if obj.isSame(parent):
    return true

  for p in obj.parents:
    if inheritsFrom(p, parent):
      return true

  return false

# Display helpers
proc printObject*(obj: ProtoObject, indent: int = 0): string =
  ## Pretty print object structure
  let spaces = repeat(' ', indent * 2)
  var result = spaces & "Object"

  if obj.tags.len > 0:
    result.add(" [" & obj.tags.join(", ") & "]")
  result.add("\n")

  if obj.properties.len > 0:
    result.add(spaces & "  properties:\n")
    for key, val in obj.properties:
      result.add(spaces & "    " & key & ": " & val.toString() & "\n")

  if obj.methods.len > 0:
    result.add(spaces & "  methods:\n")
    for selector in obj.methods.keys:
      result.add(spaces & "    " & selector & "\n")

  if obj.parents.len > 0:
    result.add(spaces & "  parents:\n")
    for parent in obj.parents:
      result.add(printObject(parent, indent + 2))

  return result

# String interpolation and formatting
proc formatString*(tmpl: string, args: Table[string, NodeValue]): string =
  ## Simple string formatting with placeholders
  result = tmpl
  for key, val in args:
    let placeholder = "{" & key & "}"
    result = result.replace(placeholder, val.toString())

# Create a simple test object hierarchy (commented out - needs proper method invocation)
# proc makeTestObjects*(): (RootObject, ProtoObject, ProtoObject) =
#   ## Create test object hierarchy for testing
#   let root = initRootObject()
#
#   # Create Animal prototype
#   let animal = newObject()
#   animal.tags = @["Animal"]
#   animal.properties = {
#     "species": NodeValue(kind: vkString, strVal: "unknown"),
#     "sound": NodeValue(kind: vkString, strVal: "silence")
#   }.toTable
#
#   # Add makeSound method
#   let makeSoundBlock = BlockNode(
#     parameters: @[],
#     temporaries: @[],
#     body: @[LiteralNode(
#       value: NodeValue(kind: vkNil)
#     )],
#     isMethod: true
#   )
#   addMethod(animal, "makeSound", makeSoundBlock)
#
#   # Create Dog instance
#   let dog = newObject()
#   dog.parents = @[animal]
#   dog.properties["species"] = NodeValue(kind: vkString, strVal: "dog")
#   dog.properties["breed"] = NodeValue(kind: vkString, strVal: "golden retriever")
#
#   return (root, animal, dog)
