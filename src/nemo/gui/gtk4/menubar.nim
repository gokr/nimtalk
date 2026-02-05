## ============================================================================
## GtkMenuBarProxy - MenuBar widget wrapper
## ============================================================================

import std/[logging, tables]
import nemo/core/types
import nemo/interpreter/evaluator
import ./ffi
import ./widget

type
  GtkMenuBarProxyObj* {.acyclic.} = object of GtkWidgetProxyObj

  GtkMenuBarProxy* = ref GtkMenuBarProxyObj

## Factory: Create new menu bar proxy
proc newGtkMenuBarProxy*(widget: GtkMenuBar, interp: ptr Interpreter): GtkMenuBarProxy =
  result = GtkMenuBarProxy()

## Factory: create a new menu bar
proc createGtkMenuBar*(interp: var Interpreter): NodeValue =
  ## Create a new GTK menu bar and return a proxy object
  let widget = gtkMenuBarNew()
  let proxy = newGtkMenuBarProxy(widget, addr(interp))

  # Look up the GtkMenuBar class
  var menuBarClass: Class = nil
  if "GtkMenuBar" in interp.globals[]:
    let val = interp.globals[]["GtkMenuBar"]
    if val.kind == vkClass:
      menuBarClass = val.classVal
  if menuBarClass == nil and "GtkWidget" in interp.globals[]:
    let val = interp.globals[]["GtkWidget"]
    if val.kind == vkClass:
      menuBarClass = val.classVal
  if menuBarClass == nil:
    menuBarClass = objectClass

  let obj = newInstance(menuBarClass)
  obj.isNimProxy = true
  obj.nimValue = cast[pointer](proxy)

  # Keep proxy alive in GC when stored as raw pointer in nimValue
  GC_ref(cast[ref RootObj](proxy))

  return obj.toValue()

## Native method: new (class method)
proc menuBarNewImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  createGtkMenuBar(interp)

## Native method: append:
proc menuBarAppendImpl*(interp: var Interpreter, self: Instance, args: seq[NodeValue]): NodeValue =
  ## Append a menu item to the menu bar
  echo "** menuBarAppendImpl: called"
  echo("  args.len=", args.len)

  if args.len < 1:
    echo("  -> no args, returning nil")
    return nilValue()

  if not (self.isNimProxy and self.nimValue != nil):
    echo("  -> self not a proxy or nilValue, isNimProxy=", self.isNimProxy)
    return nilValue()

  let menuItemVal = args[0]
  echo("  menuItemVal.kind=", menuItemVal.kind, " isNimProxy=", menuItemVal.instVal.isNimProxy)
  if menuItemVal.kind != vkInstance or not menuItemVal.instVal.isNimProxy:
    echo("  -> menuItem not an instance or not a proxy")
    return nilValue()

  let menuBarProxy = cast[GtkMenuBarProxy](self.nimValue)
  echo("  menuBarProxy.widget=", repr(menuBarProxy.widget))
  if menuBarProxy.widget == nil:
    echo("  -> menuBar.widget is nil")
    return nilValue()

  let menuItemProxy = cast[GtkWidgetProxy](menuItemVal.instVal.nimValue)
  echo("  menuItemProxy.widget=", repr(menuItemProxy.widget))
  if menuItemProxy.widget == nil:
    echo("  -> menuItem.widget is nil")
    return nilValue()

  echo("  Calling gtkBoxAppend...")
  when defined(gtk4):
    # GTK4: use gtkBoxAppend since GtkMenuBar is a GtkBox
    gtkBoxAppend(cast[GtkBox](menuBarProxy.widget), menuItemProxy.widget)
    # In GTK4, widgets need to be explicitly shown
    gtkWidgetShow(menuItemProxy.widget)
    echo("  -> appended and showed widget")
  else:
    # GTK3: use gtkShellAppend
    gtkShellAppend(menuBarProxy.widget, menuItemProxy.widget)

  nilValue()
