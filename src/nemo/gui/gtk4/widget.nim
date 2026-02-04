## ============================================================================
## GtkWidgetProxy - Base widget wrapper
## ============================================================================

import std/[logging, tables]
import nemo/core/types
import nemo/interpreter/evaluator
import ./ffi

## Signal handler callback type
type
  SignalHandler* = object
    blockNode*: BlockNode
    interp*: ptr Interpreter

## GtkWidgetProxy - base class for all GTK widget proxies
type
  GtkWidgetProxyObj* {.acyclic.} = object of RootObj
    widget*: GtkWidget
    interp*: ptr Interpreter
    signalHandlers*: Table[string, seq[SignalHandler]]
    destroyed*: bool

  GtkWidgetProxy* = ref GtkWidgetProxyObj

## Create a new widget proxy
proc newGtkWidgetProxy*(widget: GtkWidget, interp: ptr Interpreter): GtkWidgetProxy =
  result = GtkWidgetProxy(
    widget: widget,
    interp: interp,
    signalHandlers: initTable[string, seq[SignalHandler]](),
    destroyed: false
  )

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
  ## Connect a signal to a block - stub implementation
  nilValue()
