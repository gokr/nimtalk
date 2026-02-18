## ============================================================================
## BitBarrel Integration Tests
## Tests for BitBarrel database integration with Harding
## ============================================================================

when defined(bitbarrel):
  import std/[unittest, os, strutils]
  import ../src/harding/core/types
  import ../src/harding/interpreter/vm
  import ../src/harding/bitbarrel/bridge

  suite "BitBarrel Integration":
    var interp: Interpreter

    setup:
      # Initialize interpreter with standard library
      interp = createInterpreter()
      loadStdlib(interp)
      initBitBarrelBridge(interp)

    teardown:
      discard

    test "Barrel class is registered":
      check "Barrel" in interp.globals[]
      let barrelVal = interp.globals[]["Barrel"]
      check barrelVal.kind == vkClass
      let barrelCls = barrelVal.classVal
      check barrelCls.name == "Barrel"
      check barrelCls.isNimProxy == true

    test "BarrelTable class is registered":
      check "BarrelTable" in interp.globals[]
      let tableVal = interp.globals[]["BarrelTable"]
      check tableVal.kind == vkClass
      let tableCls = tableVal.classVal
      check tableCls.name == "BarrelTable"
      check tableCls.isNimProxy == true

    test "BarrelSortedTable class is registered":
      check "BarrelSortedTable" in interp.globals[]
      let sortedVal = interp.globals[]["BarrelSortedTable"]
      check sortedVal.kind == vkClass
      let sortedCls = sortedVal.classVal
      check sortedCls.name == "BarrelSortedTable"
      check sortedCls.isNimProxy == true

else:
  echo "BitBarrel tests skipped (compile with -d:bitbarrel)"
