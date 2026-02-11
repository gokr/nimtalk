## Tests for Smalltalk message precedence rules
## Precedence: unary > binary > keyword (left-to-right for same type)
##
## These tests validate that:
## 1. Unary messages have higher precedence than binary
## 2. Binary operators are left-to-right associative
## 3. Binary operators have higher precedence than keyword messages
## 4. Arguments to keyword messages can contain binary expressions

import std/[unittest, strutils]
import ../src/harding/core/types
import ../src/harding/interpreter/[vm, objects]

suite "Message Precedence: Unary > Binary > Keyword":
  var interp: Interpreter

  setup:
    interp = newInterpreter()
    initGlobals(interp)
    initSymbolTable()

  test "unary has higher precedence than binary":
    # obj foo + bar should parse as (obj foo) + bar, not obj (foo + bar)
    let result = interp.evalStatements("""
    TestObj := Object derive.
    TestObj >> value [ ^10 ].
    TestObj >> double [ ^self value * 2 ].

    obj := TestObj new.
    result := obj double + 5
    """)

    if result[1].len > 0:
      stderr.writeLine("Unary > Binary ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check(result[0][^1].intVal == 25)  # (10 * 2) + 5 = 25

  test "binary operators are left-to-right associative":
    # a + b * c should parse as (a + b) * c, not a + (b * c)
    let result = interp.evalStatements("""
    # Left-to-right: (10 + 5) * 2 = 30, not 10 + (5 * 2) = 20
    result := 10 + 5 * 2
    """)

    if result[1].len > 0:
      stderr.writeLine("Binary left-to-right ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check(result[0][^1].intVal == 30)  # (10 + 5) * 2 = 30

  test "binary has higher precedence than keyword":
    # arr at: 1 + 2 should be arr at: (1 + 2) = arr at: 3 = 4
    let result = interp.evalStatements("""
    arr := #(1 2 3 4 5).
    result := arr at: 1 + 2
    """)

    if result[1].len > 0:
      stderr.writeLine("Binary > Keyword ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check(result[0][^1].intVal == 4)  # arr at: (1 + 2) = arr at: 3 = 4

  test "comma binary operator in keyword argument":
    # The user's reported failing case:
    # Stdout writeline: "Hey", (3 + 4) printString
    # Should parse as: Stdout writeline: ("Hey" , ((3 + 4) printString))
    # NOT as: (Stdout writeline: "Hey") , ((3 + 4) printString)
    let result = interp.evalStatements("""
    Output := Object derive: #(buffer).
    Output >> initialize [ self at: #buffer put: '' ].
    Output >> writeline: msg [
        self at: #buffer put: ((self at: #buffer) , msg , '\\n')
    ].
    Output >> buffer [ ^self at: #buffer ].

    out := Output new.
    out initialize.

    # This should concatenate "A" and "B" first, then pass to writeline:
    # out writeline: "A" , "B" should be out writeline: ("A" , "B")
    out writeline: "A" , "B".
    result := out buffer
    """)

    if result[1].len > 0:
      stderr.writeLine("Comma in keyword arg ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      # Should be "AB\n" not just "A\n"
      check("AB" in result[0][^1].strVal)

  test "unary then binary then keyword":
    # Complex chain: obj foo + bar at: baz
    # Should be: ((obj foo) + bar) at: baz
    let result = interp.evalStatements("""
    Container := Table derive: #(items).
    Container >> items [ ^self at: #items ].

    c := Container new.
    c at: #items put: #(1 2 3).
    # c items at: 2 should be #(1 2 3) at: 2 = 3
    result := c items at: 2
    """)

    if result[1].len > 0:
      stderr.writeLine("Unary+binary+keyword ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check(result[0][^1].intVal == 3)

  test "multiple unary messages left-to-right":
    # obj foo bar baz should be ((obj foo) bar) baz
    let result = interp.evalStatements("""
    Chain := Object derive.
    Chain >> first [ ^self ].
    Chain >> second [ ^self ].
    Chain >> third [ ^42 ].

    c := Chain new.
    result := c first second third
    """)

    if result[1].len > 0:
      stderr.writeLine("Unary left-to-right ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check(result[0][^1].intVal == 42)

  test "parentheses override precedence":
    # obj foo: (bar + baz) should have parentheses evaluated first
    let result = interp.evalStatements("""
    MathObj := Object derive.
    MathObj >> add: a and: b [ ^a + b ].

    m := MathObj new.
    # With parens: 3 + 4 = 7, then add: 7 and: 5 = 12
    result := m add: (3 + 4) and: 5
    """)

    if result[1].len > 0:
      stderr.writeLine("Parentheses ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check(result[0][^1].intVal == 12)

  test "keyword message with complex binary expression argument":
    # obj key: a + b * c should be obj key: ((a + b) * c)
    let result = interp.evalStatements("""
    Box := Object derive: #(value).
    Box >> value: v [ self at: #value put: v ].
    Box >> value [ ^self at: #value ].

    b := Box new.
    # b value: 2 + 3 * 4 should set value to (2 + 3) * 4 = 20
    b value: 2 + 3 * 4.
    result := b value
    """)

    if result[1].len > 0:
      stderr.writeLine("Keyword with binary ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check(result[0][^1].intVal == 20)

  test "explicit parentheses work as workaround":
    # The user's workaround: Stdout writeline: ("Hey", (3 + 4) printString)
    # This should always work
    let result = interp.evalStatements("""
    Output := Object derive: #(buffer).
    Output >> initialize [ self at: #buffer put: '' ].
    Output >> writeline: msg [
        self at: #buffer put: ((self at: #buffer) , msg , '\\n')
    ].
    Output >> buffer [ ^self at: #buffer ].

    out := Output new.
    out initialize.

    # With explicit parentheses
    out writeline: ("A" , "B").
    result := out buffer
    """)

    if result[1].len > 0:
      stderr.writeLine("Explicit parens ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check("AB" in result[0][^1].strVal)

  test "binary minus in keyword argument":
    # arr at: 10 - 3 should be arr at: (10 - 3) = arr at: 7
    let result = interp.evalStatements("""
    arr := #(10 20 30 40 50 60 70 80).
    result := arr at: 10 - 3
    """)

    if result[1].len > 0:
      stderr.writeLine("Binary minus ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check(result[0][^1].intVal == 70)  # arr at: 7 = 70

  test "arithmetic in keyword arguments":
    # Multiple arithmetic operations in keyword argument
    let result = interp.evalStatements("""
    Calculator := Object derive.
    Calculator >> calculate: expr [ ^expr ].

    calc := Calculator new.
    # Should compute: ((10 + 5) * 2) - 3 = 27
    result := calc calculate: 10 + 5 * 2 - 3
    """)

    if result[1].len > 0:
      stderr.writeLine("Arithmetic ERROR: ", result[1])
    check(result[1].len == 0)
    if result[0].len >= 1:
      check(result[0][^1].intVal == 27)  # ((10 + 5) * 2) - 3 = 27
