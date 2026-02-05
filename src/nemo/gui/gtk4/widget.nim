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
    let result = invokeBlock(interp[], blockNode, @[])
    discard result  # Signal callbacks generally ignore return values
  except Exception as e:
    # Log error but don't crash the GUI
    error("Error in signal callback for '", data.signalName, "': ", e.msg)

## GtkWidgetProxy - base class for all GTK widget proxies
type
  GtkWidgetProxyObj* {.acyclic.} = object of RootObj
    widget*: GtkWidget
    interp*: ptr Interpreter
    signalHandlers*: Table[string, seq[SignalHandler]]
    destroyed*: bool

  GtkWidgetProxy* = ref GtkWidgetProxyObj

## Create a new widget proxy - automatically handles GC reference
## When the proxy is stored as a raw pointer in Instance.nimValue,
## we need to keep it alive since the GC won't track raw pointers.
proc newGtkWidgetProxy*(widget: GtkWidget, interp: ptr Interpreter): GtkWidgetProxy =
  result = GtkWidgetProxy(
    widget: widget,
    interp: interp,
    signalHandlers: initTable[string, seq[SignalHandler]](),
    destroyed: false
  )

## Keep a widget proxy alive when stored as raw pointer in Instance.nimValue
## Call this after creating a proxy that will be stored in Instance.nimValue
proc keepProxyAlive*(proxy: ref RootObj) {.inline.} =
  ## Increment GC ref count to prevent collection when stored as raw pointer
  GC_ref(proxy)

## Native method: show
proc widgetShowImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkWidgetProxy](self.nimValue)
    if proxy.widget != nil:
      gtkWidgetShow(proxy.widget)
  nilValue()

## Native method: hide
proc widgetHideImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkWidgetProxy](self.nimValue)
    if proxy.widget != nil:
      gtkWidgetHide(proxy.widget)
  nilValue()

## Native method: setSizeRequest:
proc widgetSetSizeRequestImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if args.len < 2:
    return nilValue()

  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkWidgetProxy](self.nimValue)
    let width = args[0].intVal
    let height = args[1].intVal
    if proxy.widget != nil:
      gtkWidgetSetSizeRequest(proxy.widget, width.cint, height.cint)

  nilValue()

## Native method: addCssClass:
proc widgetAddCssClassImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  when defined(gtk4):
    if args.len < 1 or args[0].kind != vkString:
      return nilValue()

    if self.isNimProxy and self.nimValue != nil:
      let proxy = cast[GtkWidgetProxy](self.nimValue)
      let cssClass = args[0].strVal
      if proxy.widget != nil:
        gtkWidgetAddCssClass(proxy.widget, cssClass.cstring)

  nilValue()

## Native method: removeCssClass:
proc widgetRemoveCssClassImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  when defined(gtk4):
    if args.len < 1 or args[0].kind != vkString:
      return nilValue()

    if self.isNimProxy and self.nimValue != nil:
      let proxy = cast[GtkWidgetProxy](self.nimValue)
      let cssClass = args[0].strVal
      if proxy.widget != nil:
        gtkWidgetRemoveCssClass(proxy.widget, cssClass.cstring)

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

  let proxy = cast[GtkWidgetProxy](self.nimValue)
  if proxy.widget == nil:
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
  let gObject = cast[GObject](proxy.widget)
  discard gSignalConnect(gObject, signalName.strVal.cstring,
                         cast[GCallback](signalCallbackProc), cast[pointer](callbackData))

  debug("Connected signal '", signalName.strVal, "' on widget")

  nilValue()
