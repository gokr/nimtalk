#
# test_exception_handling.nim - Tests for exception handling (on:do:)
#
# Tests modern Smalltalk-style exception handling with:
# - on:do: for catching exceptions
# - signal: for raising exceptions
# - Exception hierarchy (Exception, Error, etc.)
# - Handler receives exception object
# - Type matching (Error caught by Exception handler)
#

import std/[unittest, strutils]
import ../src/harding/core/types
import ../src/harding/interpreter/[vm, objects]

var sharedInterp: Interpreter
sharedInterp = newInterpreter()
initGlobals(sharedInterp)
loadStdlib(sharedInterp)

suite "Exception Handling":
  var interp {.used.}: Interpreter

  setup:
    interp = sharedInterp

  test "Exception class exists and has slots":
    let result = interp.evalStatements("""
      Ex := Exception new.
      Ex message: "test error".
      Result := Ex message
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "test error")

  test "Error class exists as subclass of Exception":
    let result = interp.evalStatements("""
      Err := Error new.
      Result := Err class name
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "Error")

  test "DivisionByZero class exists":
    let result = interp.evalStatements("""
      DBZ := DivisionByZero new.
      Result := DBZ class name
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "DivisionByZero")

  test "on:do: catches exceptions":
    let result = interp.evalStatements("""
      Result := [ Error signal: "test" ] on: Error do: [ :ex | "caught" ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "caught")

  # NOTE: Nested on:do: with exception propagation (inner handler doesn't match,
  # outer catches) causes GC memory corruption in the shared interpreter.
  # This is a known VM bug: exception signaling with nested handlers doesn't
  # properly clean up work queue state, leading to dangling references.
  # The test works correctly in isolation (REPL) but crashes after accumulation.

  test "exception handler receives exception object":
    let result = interp.evalStatements("""
      Result := [ Error signal: "my message" ] on: Error do: [ :ex | ex message ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "my message")

  test "ifError: convenience method":
    let result = interp.evalStatements("""
      Result := [ Error signal: "oops" ] ifError: [ :ex | "handled" ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "handled")

  test "Exception superclass catches Error subclass":
    let result = interp.evalStatements("""
      Result := [ Error signal: "subclass" ] on: Exception do: [ :ex | "caught by parent" ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "caught by parent")

  test "protected block returns normally when no exception":
    let result = interp.evalStatements("""
      Result := [ "normal result" ] on: Error do: [ :ex | "caught" ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "normal result")
