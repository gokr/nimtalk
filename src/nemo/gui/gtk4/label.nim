## ============================================================================
## GtkLabelProxy - Label widget wrapper
## ============================================================================

import std/[logging, tables]
import nemo/core/types
import nemo/interpreter/evaluator
import ./ffi
import ./widget

type
  GtkLabelProxyObj* {.acyclic.} = object of GtkWidgetProxyObj

  GtkLabelProxy* = ref GtkLabelProxyObj

## Factory: Create new label proxy
proc newGtkLabelProxy*(widget: GtkLabel, interp: ptr Interpreter): GtkLabelProxy =
  result = GtkLabelProxy()

## Native class method: new
proc labelNewImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  ## Create a new empty label
  let widget = gtkLabelNew(nil)
  let proxy = newGtkLabelProxy(widget, addr(interp))

  var cls: Class = nil
  if "GtkLabel" in interp.globals[]:
    let val = interp.globals[]["GtkLabel"]
    if val.kind == vkClass:
      cls = val.classVal
  if cls == nil and "GtkWidget" in interp.globals[]:
    let val = interp.globals[]["GtkWidget"]
    if val.kind == vkClass:
      cls = val.classVal
  if cls == nil:
    cls = objectClass

  let obj = newInstance(cls)
  obj.isNimProxy = true
  obj.nimValue = cast[pointer](proxy)
  GC_ref(cast[ref RootObj](proxy))
  return obj.toValue()

## Native class method: newLabel:
proc labelNewLabelImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  ## Create a new label with text
  if args.len < 1 or args[0].kind != vkString:
    return nilValue()

  let widget = gtkLabelNew(args[0].strVal.cstring)
  let proxy = newGtkLabelProxy(widget, addr(interp))

  var cls: Class = nil
  if "GtkLabel" in interp.globals[]:
    let val = interp.globals[]["GtkLabel"]
    if val.kind == vkClass:
      cls = val.classVal
  if cls == nil and "GtkWidget" in interp.globals[]:
    let val = interp.globals[]["GtkWidget"]
    if val.kind == vkClass:
      cls = val.classVal
  if cls == nil:
    cls = objectClass

  let obj = newInstance(cls)
  obj.isNimProxy = true
  obj.nimValue = cast[pointer](proxy)
  GC_ref(cast[ref RootObj](proxy))
  return obj.toValue()

## Native instance method: text:
proc labelSetTextImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  ## Set label text
  if args.len < 1 or args[0].kind != vkString:
    return nilValue()

  if not (self.isNimProxy and self.nimValue != nil):
    return nilValue()

  let proxy = cast[GtkLabelProxy](self.nimValue)
  if proxy.widget == nil:
    return nilValue()

  gtkLabelSetText(cast[GtkLabel](proxy.widget), args[0].strVal.cstring)

  debug("Set label text to '", args[0].strVal, "'")

  nilValue()

## Native instance method: text (getter)
proc labelGetTextImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  ## Get label text
  if not (self.isNimProxy and self.nimValue != nil):
    return nilValue()

  let proxy = cast[GtkLabelProxy](self.nimValue)
  if proxy.widget == nil:
    return "".toValue()

  let text = gtkLabelGetText(cast[GtkLabel](proxy.widget))
  if text == nil:
    return "".toValue()

  result = toValue($text)
