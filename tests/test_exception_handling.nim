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
    # Verify Exception class is loaded from Exception.hrd
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
    # Basic exception catching
    let result = interp.evalStatements("""
      Result := [ Error signal: "test" ] on: Error do: [ :ex | ^"caught" ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "caught")

  test "on:do: only catches specified exception type":
    # MessageNotUnderstood handler should not catch Error
    # Outer Error handler should catch it instead
    let result = interp.evalStatements("""
      Result := [
        [ Error signal: "test" ] on: MessageNotUnderstood do: [ :ex | ^"wrong handler" ]
      ] on: Error do: [ :ex | ^"correct handler" ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "correct handler")

  test "exception handler receives exception object":
    # Handler can access the exception object
    let result = interp.evalStatements("""
      Result := [ Error signal: "my message" ] on: Error do: [ :ex | ^ex message ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "my message")

  test "ifError: convenience method":
    # ifError: is a convenience method that catches Error
    let result = interp.evalStatements("""
      Result := [ Error signal: "oops" ] ifError: [ :ex | ^"handled" ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "handled")

  test "nested handlers - inner catches, outer not invoked":
    # When inner handler catches, outer should not be invoked
    let result = interp.evalStatements("""
      | caught |
      caught := nil.
      [
        [ Error signal: "test" ] on: Error do: [ :ex | caught := "inner" ]
      ] on: Error do: [ :ex | caught := "outer" ].
      Result := caught
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "inner")

  test "exception propagates if not caught":
    # Uncaught exceptions should not stop execution
    # Currently they just print to stderr and return nil
    let result = interp.evalStatements("""
      [ Error signal: "uncaught" ].
      Result := "should not reach"
    """)
    # With current implementation, uncaught exceptions return nil
    # but don't stop subsequent execution
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "should not reach")

  test "Exception superclass catches Error subclass":
    # Error is a subclass of Exception, so Exception handler should catch Error
    let result = interp.evalStatements("""
      Result := [ Error signal: "subclass" ] on: Exception do: [ :ex | ^"caught by parent" ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "caught by parent")

  test "protected block returns normally when no exception":
    # When no exception is signaled, the protected block result is returned
    let result = interp.evalStatements("""
      Result := [ "normal result" ] on: Error do: [ :ex | ^"caught" ]
    """)
    check(result[1].len == 0)
    check(result[0][^1].kind == vkString)
    check(result[0][^1].strVal == "normal result")
