# GTK Bridge Architecture

This document explains how the Nemo GTK bridge works and how to create GTK widgets from Nemo code.

## Overview

The GTK bridge provides a way to create and manipulate GTK widgets from Nemo code. It consists of several layers that work together to provide seamless integration between Nemo objects and GTK widgets.

## File Structure

### Core Files

#### `ffi.nim` - Foreign Function Interface
- Provides raw bindings to GTK C functions
- Defines types like `GtkWidget`, `GtkWindow`, `GtkButton`
- Handles both GTK3 and GTK4 via compile-time flags (`-d:gtk4`)
- Contains function declarations like `gtkWindowNew()`, `gtkBoxAppend()`, etc.

```nim
# Example from ffi.nim
proc gtkWindowNew*(): GtkWindow {.cdecl, importc: "gtk_window_new".}
proc gtkBoxAppend*(box: GtkBox, child: GtkWidget) {.cdecl, importc: "gtk_box_append".}
```

#### `widget.nim` - Base Widget Proxy
- Defines `GtkWidgetProxy` - the base type for all widget proxies
- Contains `proxyTable` - global table mapping widget pointers to proxies
- Contains `instanceWidgetTable` - maps Instance addresses to widget pointers
- Provides signal handling infrastructure

```nim
type
  GtkWidgetProxyObj = object of RootObj
    widget*: GtkWidget
    interp*: ptr Interpreter
    signalHandlers*: Table[string, seq[SignalHandler]]
```

#### `bridge.nim` - Class Registration
- Registers all GTK classes with the Nemo interpreter
- Creates Nemo classes (GtkWidget, GtkWindow, GtkButton, etc.)
- Maps native methods (Nim functions) to Nemo method names
- Loads wrapper files from `lib/nemo/gui/Gtk4/`

```nim
# Register GtkWindow class
let windowCls = newClass(superclasses = @[widgetCls], name = "GtkWindow")
let windowPresentMethod = createCoreMethod("present")
windowPresentMethod.nativeImpl = cast[pointer](windowPresentImpl)
addMethodToClass(windowCls, "present", windowPresentMethod)
```

### Widget-Specific Files

Each widget type has its own file:

- `window.nim` - GtkWindow (title, setDefaultSize, present, close)
- `button.nim` - GtkButton (label, clicked)
- `box.nim` - GtkBox (append, prepend, setSpacing)
- `menubar.nim`, `menuitem.nim` - Menu widgets
- `textview.nim`, `textbuffer.nim` - Text editing widgets

## Architecture

### Two-Table System

The bridge uses two complementary tables to solve the GC issue:

1. **`proxyTable: Table[GtkWidget, GtkWidgetProxy]`**
   - Maps widget pointers to their proxy objects
   - Used for signal handling and looking up proxy data

2. **`instanceWidgetTable: Table[int, GtkWidget]`**
   - Maps Instance addresses (as integers) to widget pointers
   - Solves the problem of Instance identity across copies

### Why Two Tables?

When a Nemo Instance is passed around, the GC may create copies that have different addresses but refer to the same logical object. By storing widgets keyed by Instance address, we ensure we can always find the correct widget even if the Instance was copied.

### Widget Creation Flow

1. **Nemo code calls class method** (e.g., `GtkButton newLabel: "File"`)
2. **Bridge routes to native method** (`buttonNewLabelImpl`)
3. **Factory creates GTK widget** (`gtkButtonNewWithLabel`)
4. **Create proxy and store in tables:**
   - `proxyTable[widget] = proxy`
   - `instanceWidgetTable[InstanceAddr] = widget`
5. **Return Instance to Nemo** with `isNimProxy = true`

### Signal Handling

1. Nemo code connects signal: `button clicked: [ ... ]`
2. `buttonClickedImpl` creates `SignalCallbackData` on heap
3. Calls `gSignalConnect` with C callback `signalCallbackProc`
4. When signal fires, `signalCallbackProc` invokes the Nemo block

## Creating Widgets from Nemo

### Basic Usage

```nemo
# Create window
window := GtkWindow new.
window title: "My App".
window setDefaultSize: 800 height: 600.

# Create button with click handler
button := GtkButton newLabel: "Click Me".
button clicked: [
    Transcript showCr: "Button was clicked!"
].

# Layout
box := GtkBox vertical.
box append: button.
window setChild: box.
window present.
```

### Creating Widgets Purely in Nemo (Without Native Methods)

You can construct widgets using Nemo primitives by:
1. Creating an instance of the class
2. Setting `isNimProxy` to true
3. Storing the widget pointer via primitive methods

```nemo
# Pure Nemo construction of a button (conceptual)
MyButtonBuilder>>buildButton: labelText [
    | button widgetPtr |

    # Create instance
    button := GtkButton new.

    # Create the actual GTK widget via FFI primitive
    # (This would require a primitive that calls gtk_button_new_with_label)
    widgetPtr := self ffiCall: "gtk_button_new_with_label" with: labelText.

    # Store in the instance
    button setWidgetPointer: widgetPtr.
    button isNimProxy: true.

    ^button
]
```

In practice, the FFI primitives needed for pure Nemo construction would need to be exposed. Currently, widget creation is done in Nim for efficiency and type safety.

## Adding New Widgets

To add support for a new GTK widget:

1. **Create `newwidget.nim`:**
```nim
type
  GtkNewWidgetProxyObj = object of GtkWidgetProxyObj
  GtkNewWidgetProxy = ref GtkNewWidgetProxyObj

proc newGtkNewWidgetProxy*(widget: GtkNewWidget, interp: ptr Interpreter): GtkNewWidgetProxy =
  result = GtkNewWidgetProxy(...)
  proxyTable[cast[GtkWidget](widget)] = result

proc createGtkNewWidget*(interp: var Interpreter): NodeValue =
  let widget = gtkNewWidgetNew()
  discard newGtkNewWidgetProxy(widget, addr(interp))
  # ... create instance and store in tables

proc newWidgetNewImpl*(interp: var Interpreter, ...): NodeValue =
  createGtkNewWidget(interp)
```

2. **Register in `bridge.nim`:**
```nim
let newWidgetCls = newClass(superclasses = @[widgetCls], name = "GtkNewWidget")
# Add methods...
interp.globals[]["GtkNewWidget"] = newWidgetCls.toValue()
```

3. **Create wrapper file** `lib/nemo/gui/Gtk4/NewWidget.nemo`:
```nemo
# Optional convenience methods
GtkNewWidget>>customMethod [
    # Implementation
]
```

## GTK3 vs GTK4

The bridge supports both versions:

- **GTK4** (default with `-d:gtk4`): Uses `gtk_window_set_child`, `gtk_box_append`
- **GTK3**: Uses `gtk_container_add`, `gtk_box_pack_start`

Widget files check `when defined(gtk4):` to use appropriate APIs.

## Debugging Tips

1. **Check proxy table:** Add debug prints to see if widgets are being stored/looked up
2. **Instance addresses:** Compare Instance addresses between creation and use
3. **Signal connection:** Verify `gSignalConnect` returns a non-zero handler ID
4. **GTK warnings:** Watch for `Gtk-CRITICAL` warnings about invalid widgets

## Common Issues

### "proxy is nil"
- Proxy not stored in `proxyTable` during creation
- Widget pointer type mismatch (need `cast[GtkWidget]`)

### Widget not visible
- Missing `gtkWidgetShow()` call
- Widget not added to parent container

### Signal not firing
- Block not captured correctly
- Signal name mismatch (GTK4 uses "clicked", not "activate" for buttons)

### Window doesn't exit on close
- Need to connect "destroy" signal or use `connectDestroy`
