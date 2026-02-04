## ============================================================================
## GtkWindowProxy - Window widget wrapper
## ============================================================================

import std/[logging, tables]
import nemo/core/types
import nemo/interpreter/evaluator
import ./ffi
import ./widget

## GtkWindowProxy extends GtkWidgetProxy
type
  GtkWindowProxyObj* {.acyclic.} = object of GtkWidgetProxyObj
    ## Additional window-specific fields can go here

  GtkWindowProxy* = ref GtkWindowProxyObj

## Create a new window proxy
proc newGtkWindowProxy*(window: GtkWindow, interp: ptr Interpreter): GtkWindowProxy =
  result = GtkWindowProxy(
    widget: window,
    interp: interp,
    signalHandlers: initTable[string, seq[SignalHandler]](),
    destroyed: false
  )

## Factory: create a new window
proc createGtkWindow*(interp: var Interpreter): NodeValue =
  ## Create a new GTK window and return a proxy object
  when defined(gtk4):
    let window = gtkWindowNew()
  else:
    let window = gtkWindowNew(GTKWINDOWTOPLEVEL)
  let proxy = newGtkWindowProxy(window, addr(interp))

  # Create a Nemo instance to wrap the proxy
  # We need to get the GtkWindow class from globals
  var windowClass: Class = nil
  if "GtkWindow" in interp.globals[]:
    let winVal = interp.globals[]["GtkWindow"]
    if winVal.kind == vkClass:
      windowClass = winVal.classVal

  if windowClass == nil:
    # Fallback to Object class
    windowClass = objectClass

  let obj = newInstance(windowClass)
  obj.isNimProxy = true
  obj.nimValue = cast[pointer](proxy)

  return obj.toValue()

## Native method: new (class method for Window)
proc windowNewImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  createGtkWindow(interp)

## Native method: title:
proc windowSetTitleImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if args.len < 1 or args[0].kind != vkString:
    return nilValue()

  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkWindowProxy](self.nimValue)
    let title = args[0].strVal
    if proxy.widget != nil:
      gtkWindowSetTitle(cast[GtkWindow](proxy.widget), title.cstring)

  nilValue()

## Native method: setDefaultSize:height:
proc windowSetDefaultSizeImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if args.len < 2:
    return nilValue()

  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkWindowProxy](self.nimValue)
    let width = args[0].intVal
    let height = args[1].intVal
    if proxy.widget != nil:
      gtkWindowSetDefaultSize(cast[GtkWindow](proxy.widget), width.cint, height.cint)

  nilValue()

## Native method: setChild: (GTK3/GTK4 compatible)
proc windowSetChildImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if args.len < 1 or args[0].kind != vkInstance:
    return nilValue()

  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkWindowProxy](self.nimValue)
    let childInstance = args[0].instVal

    if childInstance.isNimProxy and childInstance.nimValue != nil:
      let childProxy = cast[GtkWidgetProxy](childInstance.nimValue)
      if proxy.widget != nil and childProxy.widget != nil:
        when defined(gtk4):
          gtkWindowSetChild(cast[GtkWindow](proxy.widget), childProxy.widget)
        else:
          gtkContainerAdd(cast[GtkWindow](proxy.widget), childProxy.widget)

  nilValue()

## Native method: present
proc windowPresentImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkWindowProxy](self.nimValue)
    if proxy.widget != nil:
      gtkWindowPresent(cast[GtkWindow](proxy.widget))

  nilValue()

## Native method: close
proc windowCloseImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  if self.isNimProxy and self.nimValue != nil:
    let proxy = cast[GtkWindowProxy](self.nimValue)
    if proxy.widget != nil:
      gtkWindowClose(cast[GtkWindow](proxy.widget))

  nilValue()
