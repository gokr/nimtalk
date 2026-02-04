## ============================================================================
## GTK FFI Bindings for Nemo
## Supports both GTK3 and GTK4 via compile-time flag
## Use -d:gtk4 to compile with GTK4 support (default is GTK3)
## ============================================================================

when defined(gtk4):
  {.passl: "-lgtk-4 -lgio-2.0 -lgobject-2.0 -lglib-2.0".}
else:
  {.passl: "-lgtk-3 -lgio-2.0 -lgobject-2.0 -lglib-2.0".}

## Types
type
  GtkWidget* = pointer
  GtkWindow* = pointer
  GtkButton* = pointer
  GtkBox* = pointer
  GtkLabel* = pointer
  GtkApplication* = pointer
  GtkApplicationWindow* = pointer
  GApplication* = pointer
  GObject* = pointer
  GCallback* = pointer
  GClosureNotify* = pointer

  GType* = csize_t
  GConnectFlags* = cint

const
  GCONNECTAFTER* = 1.GConnectFlags
  GCONNECTSWAPPED* = 2.GConnectFlags

when defined(gtk4):
  # GTK4 specific imports
  proc gtkWindowNew*(): GtkWindow {.cdecl, importc: "gtk_window_new".}
  proc gtkWindowSetChild*(window: GtkWindow, child: GtkWidget) {.cdecl, importc: "gtk_window_set_child".}
  proc gtkBoxAppend*(box: GtkBox, child: GtkWidget) {.cdecl, importc: "gtk_box_append".}
  proc gtkBoxPrepend*(box: GtkBox, child: GtkWidget) {.cdecl, importc: "gtk_box_prepend".}
  proc gtkWidgetAddCssClass*(widget: GtkWidget, cssClass: cstring) {.cdecl, importc: "gtk_widget_add_css_class".}
  proc gtkWidgetRemoveCssClass*(widget: GtkWidget, cssClass: cstring) {.cdecl, importc: "gtk_widget_remove_css_class".}
  proc gtkInit*() {.cdecl, importc: "gtk_init".}

  const
    GTKORIENTATIONHORIZONTAL* = 0.cint
    GTKORIENTATIONVERTICAL* = 1.cint

else:
  # GTK3 specific imports
  proc gtkWindowNew*(windowType: cint): GtkWindow {.cdecl, importc: "gtk_window_new".}
  proc gtkContainerAdd*(container: pointer, widget: GtkWidget) {.cdecl, importc: "gtk_container_add".}
  proc gtkBoxPackStart*(box: GtkBox, child: GtkWidget, expand: cint, fill: cint, padding: cuint) {.cdecl, importc: "gtk_box_pack_start".}
  proc gtkBoxPackEnd*(box: GtkBox, child: GtkWidget, expand: cint, fill: cint, padding: cuint) {.cdecl, importc: "gtk_box_pack_end".}

  const
    GTKWINDOWTOPLEVEL* = 0.cint
    GTKORIENTATIONHORIZONTAL* = 0.cint
    GTKORIENTATIONVERTICAL* = 1.cint

  proc gtkInit*(argc: pointer, argv: pointer) {.cdecl, importc: "gtk_init".}

# Common GTK functions (both versions)
proc gtkWindowSetTitle*(window: GtkWindow, title: cstring) {.cdecl, importc: "gtk_window_set_title".}
proc gtkWindowSetDefaultSize*(window: GtkWindow, width: cint, height: cint) {.cdecl, importc: "gtk_window_set_default_size".}
proc gtkWindowPresent*(window: GtkWindow) {.cdecl, importc: "gtk_window_present".}
proc gtkWindowClose*(window: GtkWindow) {.cdecl, importc: "gtk_window_close".}

# GTK Button
proc gtkButtonNew*(): GtkButton {.cdecl, importc: "gtk_button_new".}
proc gtkButtonNewWithLabel*(label: cstring): GtkButton {.cdecl, importc: "gtk_button_new_with_label".}
proc gtkButtonSetLabel*(button: GtkButton, label: cstring) {.cdecl, importc: "gtk_button_set_label".}
proc gtkButtonGetLabel*(button: GtkButton): cstring {.cdecl, importc: "gtk_button_get_label".}

# GTK Box
proc gtkBoxNew*(orientation: cint, spacing: cint): GtkBox {.cdecl, importc: "gtk_box_new".}
proc gtkBoxSetSpacing*(box: GtkBox, spacing: cint) {.cdecl, importc: "gtk_box_set_spacing".}

# GTK Label
proc gtkLabelNew*(text: cstring): GtkLabel {.cdecl, importc: "gtk_label_new".}
proc gtkLabelSetText*(label: GtkLabel, text: cstring) {.cdecl, importc: "gtk_label_set_text".}
proc gtkLabelGetText*(label: GtkLabel): cstring {.cdecl, importc: "gtk_label_get_text".}

# GTK Widget (base)
proc gtkWidgetShow*(widget: GtkWidget) {.cdecl, importc: "gtk_widget_show".}
proc gtkWidgetHide*(widget: GtkWidget) {.cdecl, importc: "gtk_widget_hide".}
proc gtkWidgetShowAll*(widget: GtkWidget) {.cdecl, importc: "gtk_widget_show_all".}
proc gtkWidgetSetSizeRequest*(widget: GtkWidget, width: cint, height: cint) {.cdecl, importc: "gtk_widget_set_size_request".}

# GObject signals
proc gSignalConnectData*(instance: GObject, detailedSignal: cstring, cHandler: GCallback,
                         data: pointer, destroyData: GClosureNotify, connectFlags: GConnectFlags): culong {.cdecl, importc: "g_signal_connect_data".}

proc gSignalEmit*(instance: GObject, signalId: cuint, detail: cuint) {.cdecl, importc: "g_signal_emit_by_name".}

# GTK Main Loop (GTK3 only)
when not defined(gtk4):
  proc gtkMain*() {.cdecl, importc: "gtk_main".}
  proc gtkMainQuit*() {.cdecl, importc: "gtk_main_quit".}
  proc gtkMainIterationDo*(blocking: cint): cint {.cdecl, importc: "gtk_main_iteration_do".}

# GTK4 Application support
type
  GApplicationFlags* = cint

when defined(gtk4):
  proc gtkApplicationNew*(applicationId: cstring, flags: GApplicationFlags): GtkApplication {.cdecl, importc: "gtk_application_new".}
  proc gApplicationRun*(app: GApplication, argc: cint, argv: pointer): cint {.cdecl, importc: "g_application_run".}
  proc gApplicationQuit*(app: GApplication) {.cdecl, importc: "g_application_quit".}
  proc gtkApplicationWindowNew*(app: GtkApplication): GtkWindow {.cdecl, importc: "gtk_application_window_new".}

  const
    GAPPLICATIONFLAGSNONE* = 0.GApplicationFlags

# GObject reference counting
proc gObjectRef*(obj: pointer): pointer {.cdecl, importc: "g_object_ref".}
proc gObjectUnref*(obj: pointer) {.cdecl, importc: "g_object_unref".}

# Helper for signal connection
proc gSignalConnect*(instance: GObject, signal: cstring, cHandler: GCallback, data: pointer): culong =
  gSignalConnectData(instance, signal, cHandler, data, nil, 0.GConnectFlags)

# Helper to initialize GTK (works with both versions)
when defined(gtk4):
  proc initGtk*() =
    gtkInit()
else:
  proc initGtk*() =
    gtkInit(nil, nil)
