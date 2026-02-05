## ============================================================================
## GtkWidgetProxy - Base widget wrapper
## ============================================================================

import std/[logging, tables]
import nemo/core/types
import nemo/interpreter/evaluator
import ./ffi

## Signal handler callback type - stores block info for invocation
type
  SignalHandler* = object
    blockNode*: BlockNode
    interp*: ptr Interpreter

## Signal callback data passed to C callback
type
  SignalCallbackData* = object
    handler*: SignalHandler
    signalName*: string

## C callback for GTK signals - receives widget and user data
proc signalCallbackProc*(widget: GtkWidget, userData: pointer) {.cdecl.} =
  ## Called by GTK when a signal is emitted
  ## userData points to a SignalCallbackData containing the handler info
  if userData == nil:
    return

  var data = cast[ptr SignalCallbackData](userData)
  if data.handler.interp == nil or data.handler.blockNode == nil:
    return

  let interp = data.handler.interp
  let blockNode = data.handler.blockNode

  # Invoke the Nemo block with empty args (typical for signal callbacks)
  # The block captures the widget/variables it needs via closure
  try:
    echo "DEBUG signalCallback: globals count=", interp.globals[].len
    echo "DEBUG signalCallback: GtkBox in globals=", "GtkBox" in interp.globals[]
    echo "DEBUG signalCallback: Transcript in globals=", "Transcript" in interp.globals[]
    if "Transcript" in interp.globals[]:
      let transcriptVal = interp.globals[]["Transcript"]
      echo "DEBUG signalCallback: Transcript kind=", $transcriptVal.kind
    if "GtkBox" in interp.globals[]:
      let boxClass = interp.globals[]["GtkBox"]
      echo "DEBUG signalCallback: GtkBox kind=", $boxClass.kind
      if boxClass.kind == vkClass:
        echo "DEBUG signalCallback: GtkBox allClassMethods.len=", boxClass.classVal.allClassMethods.len
    let result = invokeBlock(interp[], blockNode, @[])
    echo "DEBUG signalCallback: block completed, result=", result.toString
    discard result  # Signal callbacks generally ignore return values
  except Exception as e:
    # Log error but don't crash the GUI
    echo "DEBUG signalCallback: ERROR: ", e.msg
    error("Error in signal callback for '", data.signalName, "': ", e.msg)

## GtkWidgetProxy - base class for all GTK widget proxies
type
  GtkWidgetProxyObj* {.acyclic.} = object of RootObj
    widget*: GtkWidget
    interp*: ptr Interpreter
    signalHandlers*: Table[string, seq[SignalHandler]]
    destroyed*: bool

  GtkWidgetProxy* = ref GtkWidgetProxyObj

## Global proxy table - maps widget pointers to their proxies
## This avoids storing ref objects as raw pointers (which GC can move)
var proxyTable* {.global.}: Table[GtkWidget, GtkWidgetProxy] = initTable[GtkWidget, GtkWidgetProxy]()

## Create a new widget proxy - stores in global table instead of raw pointer
proc newGtkWidgetProxy*(widget: GtkWidget, interp: ptr Interpreter): GtkWidgetProxy =
  result = GtkWidgetProxy(
    widget: widget,
    interp: interp,
    signalHandlers: initTable[string, seq[SignalHandler]](),
    destroyed: false
  )
  # Store in global table keyed by widget pointer
  proxyTable[widget] = result

## Get a proxy for a widget from the global table
proc getGtkWidgetProxy*(widget: GtkWidget): GtkWidgetProxy =
  if widget in proxyTable:
    return proxyTable[widget]
  return nil

## Remove a proxy from the global table
proc removeGtkWidgetProxy*(widget: GtkWidget) =
  if widget in proxyTable:
    proxyTable.del(widget)

## Alternative: Store widget pointer keyed by Instance address
## This is more reliable since Instance (ref) identity is preserved
var instanceWidgetTable* {.global.}: Table[int, GtkWidget] = initTable[int, GtkWidget]()

## Store widget for an instance
proc storeInstanceWidget*(inst: Instance, widget: GtkWidget) =
  let key = cast[int](inst)
  instanceWidgetTable[key] = widget

## Retrieve widget for an instance
proc getInstanceWidget*(inst: Instance): GtkWidget =
  let key = cast[int](inst)
  if key in instanceWidgetTable:
    return instanceWidgetTable[key]
  return nil

## Native method: show
proc widgetShowImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if self.isNimProxy and self.nimValue != nil:
    let widget = cast[GtkWidget](self.nimValue)
    gtkWidgetShow(widget)
  nilValue()

## Native method: hide
proc widgetHideImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if self.isNimProxy and self.nimValue != nil:
    let widget = cast[GtkWidget](self.nimValue)
    gtkWidgetHide(widget)
  nilValue()

## Native method: setSizeRequest:
proc widgetSetSizeRequestImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if args.len < 2:
    return nilValue()

  if self.isNimProxy and self.nimValue != nil:
    let widget = cast[GtkWidget](self.nimValue)
    let width = args[0].intVal
    let height = args[1].intVal
    gtkWidgetSetSizeRequest(widget, width.cint, height.cint)

  nilValue()

## Native method: addCssClass:
proc widgetAddCssClassImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  when defined(gtk4):
    if args.len < 1 or args[0].kind != vkString:
      return nilValue()

    if self.isNimProxy and self.nimValue != nil:
      let widget = cast[GtkWidget](self.nimValue)
      let cssClass = args[0].strVal
      gtkWidgetAddCssClass(widget, cssClass.cstring)

  nilValue()

## Native method: removeCssClass:
proc widgetRemoveCssClassImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  when defined(gtk4):
    if args.len < 1 or args[0].kind != vkString:
      return nilValue()

    if self.isNimProxy and self.nimValue != nil:
      let widget = cast[GtkWidget](self.nimValue)
      let cssClass = args[0].strVal
      gtkWidgetRemoveCssClass(widget, cssClass.cstring)

  nilValue()

## Native method: connect:do:
proc widgetConnectDoImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  ## Connect a signal to a block
  ## Takes two arguments: signal name (string) and block to execute
  if args.len < 2:
    return nilValue()

  if not (self.isNimProxy and self.nimValue != nil):
    return nilValue()

  let signalName = args[0]
  let blockVal = args[1]

  if signalName.kind != vkString or blockVal.kind != vkBlock:
    return nilValue()

  let widget = cast[GtkWidget](self.nimValue)

  # Look up the proxy from the global table
  let proxy = getGtkWidgetProxy(widget)
  if proxy == nil:
    return nilValue()

  # Create signal handler data - allocated on heap to persist beyond this call
  var callbackData = cast[ptr SignalCallbackData](alloc0(sizeof(SignalCallbackData)))
  callbackData.handler = SignalHandler(
    blockNode: blockVal.blockVal,
    interp: addr(interp)
  )
  callbackData.signalName = signalName.strVal

  # Store in proxy's signal handlers table for reference (prevents GC cleanup)
  if signalName.strVal notin proxy.signalHandlers:
    proxy.signalHandlers[signalName.strVal] = @[]
  proxy.signalHandlers[signalName.strVal].add(callbackData.handler)

  # Connect the signal using GTK's g_signal_connect
  # Pass the callback data as user_data
  let gObject = cast[GObject](widget)
  discard gSignalConnect(gObject, signalName.strVal.cstring,
                         cast[GCallback](signalCallbackProc), cast[pointer](callbackData))

  debug("Connected signal '", signalName.strVal, "' on widget")

  nilValue()
