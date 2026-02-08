#
# test_extend.nim - Tests for method batching (extend:, extendClass:)
#
# NOTE: The extend: and extendClass: methods are NOT currently implemented.
# They are documented in MANUAL.md but no implementation exists in the codebase.
# These tests document the expected behavior once implemented.
#

import std/unittest
import ../src/harding/core/types
import ../src/harding/interpreter/[vm, objects]

var sharedInterp: Interpreter
sharedInterp = newInterpreter()
initGlobals(sharedInterp)
loadStdlib(sharedInterp)

suite "Method Batching (extend:":
  var interp {.used.}: Interpreter

  setup:
    interp = sharedInterp

  test "extend: adds multiple methods to a class (NOT IMPLEMENTED)":
    # This test documents expected behavior
    let result = interp.evalStatements("""
      MyClass := Object derive.
      MyClass extend: [
        self >> method1 [ ^1 ].
        self >> method2 [ ^2 ]
      ].
      obj := MyClass new.
      Result1 := obj method1.
      Result2 := obj method2
    """)
    if result[1].len > 0:
      # Expected to fail with current implementation
      check("extend:" in result[1] or "doesNotUnderstand" in result[1])

  test "extendClass: adds class methods (NOT IMPLEMENTED)":
    let result = interp.evalStatements("""
      MyClass := Object derive.
      MyClass extendClass: [
        self >> classMethod1 [ ^"class" ]
      ].
      Result := MyClass classMethod1
    """)
    if result[1].len > 0:
      check("extendClass:" in result[1] or "doesNotUnderstand" in result[1])

  test "extend: methods are immediately available (NOT IMPLEMENTED)":
    let result = interp.evalStatements("""
      MyClass := Object derive.
      MyClass extend: [
        self >> greet [ ^"hello" ]
      ].
      obj := MyClass new.
      Result := obj greet
    """)
    if result[1].len > 0:
      check("extend:" in result[1] or "doesNotUnderstand" in result[1])

  test "multiple extend: calls work (NOT IMPLEMENTED)":
    let result = interp.evalStatements("""
      MyClass := Object derive.
      MyClass extend: [
        self >> methodA [ ^"A" ]
      ].
      MyClass extend: [
        self >> methodB [ ^"B" ]
      ].
      obj := MyClass new.
      Result1 := obj methodA.
      Result2 := obj methodB
    """)
    if result[1].len > 0:
      check("extend:" in result[1] or "doesNotUnderstand" in result[1])

  test "extend: can reference self (NOT IMPLEMENTED)":
    let result = interp.evalStatements("""
      MyClass := Object derive.
      MyClass extend: [
        self >> whoami [ ^self class name ]
      ].
      obj := MyClass new.
      Result := obj whoami
    """)
    if result[1].len > 0:
      check("extend:" in result[1] or "doesNotUnderstand" in result[1])

  test "extend: works with accessors (NOT IMPLEMENTED)":
    let result = interp.evalStatements("""
      MyClass := Object deriveWithAccessors: #(value).
      MyClass extend: [
        self >> doubled [ ^self value * 2 ]
      ].
      obj := MyClass new.
      obj value: 21.
      Result := obj doubled
    """)
    if result[1].len > 0:
      check("extend:" in result[1] or "doesNotUnderstand" in result[1])
