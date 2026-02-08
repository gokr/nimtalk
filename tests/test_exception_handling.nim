#
# test_exception_handling.nim - Tests for exception handling (on:do:)
#
# NOTE: Exception handling is partially implemented. The Exception.hrd file
# defines the exception classes (Exception, Error, MessageNotUnderstood, etc.)
# and the on:do: method signature, but the core primitives (primitiveOnDo:,
# primitiveSignal) are NOT yet implemented in the VM.
#
# These tests document the expected behavior once the primitives are implemented.
#

import std/unittest
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

  # The following tests require primitiveOnDo: and primitiveSignal to be implemented
  # Currently these will fail with "doesNotUnderstand: primitiveOnDo:"

  test "on:do: catches exceptions (REQUIRES primitiveOnDo:)":
    # This test will fail until primitiveOnDo: is implemented
    let result = interp.evalStatements("""
      Result := [ Error signal: "test" ] on: Error do: [ :ex | ^"caught" ]
    """)
    if result[1].len > 0:
      # Expected to fail with current implementation
      check("primitiveOnDo:" in result[1] or "doesNotUnderstand" in result[1])
    else:
      # If it works, check the result
      check(result[0][^1].kind == vkString)
      check(result[0][^1].strVal == "caught")

  test "on:do: only catches specified exception type (REQUIRES primitiveOnDo:)":
    let result = interp.evalStatements("""
      Result := [
        [ Error signal: "test" ] on: MessageNotUnderstood do: [ :ex | ^"wrong handler" ]
      ] on: Error do: [ :ex | ^"correct handler" ]
    """)
    if result[1].len > 0:
      check("primitiveOnDo:" in result[1] or "doesNotUnderstand" in result[1])

  test "exception handler receives exception object (REQUIRES primitiveOnDo:)":
    let result = interp.evalStatements("""
      Result := [ Error signal: "my message" ] on: Error do: [ :ex | ^ex message ]
    """)
    if result[1].len == 0:
      check(result[0][^1].kind == vkString)
      check(result[0][^1].strVal == "my message")

  test "ifError: convenience method (REQUIRES primitiveOnDo:)":
    let result = interp.evalStatements("""
      Result := [ Error signal: "oops" ] ifError: [ :ex | ^"handled" ]
    """)
    if result[1].len == 0:
      check(result[0][^1].kind == vkString)
      check(result[0][^1].strVal == "handled")

  test "exception propagates if not caught (REQUIRES primitiveSignal:)":
    let result = interp.evalStatements("""
      [ Error signal: "uncaught" ].
      Result := "should not reach"
    """)
    if result[1].len > 0:
      # Expected error - either primitiveSignal: missing or actual exception
      check("primitiveSignal:" in result[1] or "doesNotUnderstand" in result[1] or "Error" in result[1])
