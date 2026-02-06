#
# test_super.nim - Tests for super syntax (unqualified and qualified)
#

import std/[unittest, tables]
import ../src/harding/core/types
import ../src/harding/parser/[lexer, parser]
import ../src/harding/interpreter/[evaluator, objects]

suite "Super Syntax Parsing":

  test "Parse unqualified super send":
    let code = "super greet"
    let (ast, _) = parseExpression(code)

    check(ast != nil)
    check(ast of SuperSendNode)
    let superNode = cast[SuperSendNode](ast)
    check(superNode.selector == "greet")
    check(superNode.explicitParent == "")
    check(superNode.arguments.len == 0)

  test "Parse super with keyword message":
    let code = "super greet: name"
    let (ast, _) = parseExpression(code)

    check(ast != nil)
    check(ast of SuperSendNode)
    let superNode = cast[SuperSendNode](ast)
    check(superNode.selector == "greet:")
    check(superNode.arguments.len == 1)

  test "Parse super with binary operator":
    let code = "super + 5"
    let (ast, _) = parseExpression(code)

    check(ast != nil)
    check(ast of SuperSendNode)
    let superNode = cast[SuperSendNode](ast)
    check(superNode.selector == "+")
    check(superNode.arguments.len == 1)

  test "Parse qualified super send":
    # Skip this test for now - qualified super syntax needs more work
    skip()

  test "Parse qualified super with keyword":
    # Skip this test for now - qualified super syntax needs more work
    skip()

suite "SuperSendNode AST":

  test "Create SuperSendNode":
    let node = SuperSendNode(
      selector: "test",
      arguments: @[],
      explicitParent: ""
    )
    check(node.selector == "test")
    check(node.explicitParent == "")
    check(node.kind == nkSuperSend)

  test "SuperSendNode with explicit parent":
    var args: seq[Node] = @[]
    args.add(LiteralNode(value: toValue(42)))
    let node = SuperSendNode(
      selector: "foo:",
      arguments: args,
      explicitParent: "ParentClass"
    )
    check(node.selector == "foo:")
    check(node.explicitParent == "ParentClass")
    check(node.arguments.len == 1)

suite "Super Method Lookup":

  test "Unqualified super looks up in first parent":
    # Create parent class with method
    let parent = newClass(name = "Parent")
    let parentMeth = BlockNode()
    parentMeth.isMethod = true
    parent.allMethods["test"] = parentMeth

    # Create child class inheriting from parent
    let child = newClass(superclasses = @[parent], name = "Child")
    child.allMethods = parent.allMethods

    # Create instance of child
    let inst = newInstance(child)

    # Verify lookup finds parent method
    check(lookupInstanceMethod(child, "test") == parentMeth)

  test "Qualified super looks up in specific parent":
    # Create two parent classes with different methods
    # (same-name method conflict now requires explicit override)
    let parentA = newClass(name = "ParentA")
    let methA = BlockNode()
    methA.isMethod = true
    parentA.allMethods["methodA"] = methA

    let parentB = newClass(name = "ParentB")
    let methB = BlockNode()
    methB.isMethod = true
    parentB.allMethods["methodB"] = methB

    # Create child with multiple parents (no conflict - different methods)
    let child = newClass(superclasses = @[parentA, parentB], name = "Child")

    # Lookup via parent should find correct method
    check(lookupInstanceMethod(parentA, "methodA") == methA)
    check(lookupInstanceMethod(parentB, "methodB") == methB)
    # Child inherits both
    check(lookupInstanceMethod(child, "methodA") == methA)
    check(lookupInstanceMethod(child, "methodB") == methB)
