## ============================================================================
## GtkBoxProxy - Box layout widget wrapper
## ============================================================================

import std/[logging, tables]
import nemo/core/types
import nemo/interpreter/evaluator
import ./ffi
import ./widget

## GtkBoxProxy extends GtkWidgetProxy
type
  GtkBoxProxyObj* {.acyclic.} = object of GtkWidgetProxyObj
    ## Additional box-specific fields can go here

  GtkBoxProxy* = ref GtkBoxProxyObj

## Create a new box proxy
proc newGtkBoxProxy*(box: GtkBox, interp: ptr Interpreter): GtkBoxProxy =
  result = GtkBoxProxy(
    widget: box,
    interp: interp,
    signalHandlers: initTable[string, seq[SignalHandler]](),
    destroyed: false
  )

## Factory: create a new box
proc createGtkBox*(interp: var Interpreter, orientation: cint, spacing: cint = 0): NodeValue =
  ## Create a new GTK box and return a proxy object
  let box = gtkBoxNew(orientation, spacing)
  let proxy = newGtkBoxProxy(box, addr(interp))

  # Look up the GtkBox class
  var boxClass: Class = nil
  if "GtkBox" in interp.globals[]:
    let boxVal = interp.globals[]["GtkBox"]
    if boxVal.kind == vkClass:
      boxClass = boxVal.classVal

  if boxClass == nil:
    boxClass = objectClass

  let obj = newInstance(boxClass)
  obj.isNimProxy = true
  obj.nimValue = cast[pointer](proxy)

  return obj.toValue()

## Native method: new (class method)
proc boxNewImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  createGtkBox(interp, GTKORIENTATIONVERTICAL, 0)

## Native method: horizontal (class method)
proc boxHorizontalImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  createGtkBox(interp, GTKORIENTATIONHORIZONTAL, 0)

## Native method: vertical (class method)
proc boxVerticalImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  createGtkBox(interp, GTKORIENTATIONVERTICAL, 0)

## Native method: newOrientation:spacing: (class method)
proc boxNewOrientationSpacingImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if args.len < 2:
    return nilValue()

  let orientation = args[0].intVal.cint
  let spacing = args[1].intVal.cint
  createGtkBox(interp, orientation, spacing)

## Native method: append:
proc boxAppendImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if args.len < 1 or args[0].kind != vkInstance:
    return nilValue()

  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkBoxProxy](self.nimValue)
    let childInstance = args[0].instVal

    if childInstance.isNimProxy and childInstance.nimValue != nil:
      let childProxy = cast[GtkWidgetProxy](childInstance.nimValue)
      if proxy.widget != nil and childProxy.widget != nil:
        when defined(gtk4):
          gtkBoxAppend(cast[GtkBox](proxy.widget), childProxy.widget)
        else:
          gtkBoxPackStart(cast[GtkBox](proxy.widget), childProxy.widget, 1, 1, 0)

  nilValue()

## Native method: prepend:
proc boxPrependImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if args.len < 1 or args[0].kind != vkInstance:
    return nilValue()

  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkBoxProxy](self.nimValue)
    let childInstance = args[0].instVal

    if childInstance.isNimProxy and childInstance.nimValue != nil:
      let childProxy = cast[GtkWidgetProxy](childInstance.nimValue)
      if proxy.widget != nil and childProxy.widget != nil:
        when defined(gtk4):
          gtkBoxPrepend(cast[GtkBox](proxy.widget), childProxy.widget)
        else:
          gtkBoxPackEnd(cast[GtkBox](proxy.widget), childProxy.widget, 1, 1, 0)

  nilValue()

## Native method: setSpacing:
proc boxSetSpacingImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if args.len < 1:
    return nilValue()

  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkBoxProxy](self.nimValue)
    let spacing = args[0].intVal
    if proxy.widget != nil:
      gtkBoxSetSpacing(cast[GtkBox](proxy.widget), spacing.cint)

  nilValue()
