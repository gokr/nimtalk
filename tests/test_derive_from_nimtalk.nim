# Test derive: from Nimtalk code
#
# This tests that we can create classes with instance variables
# directly from Nimtalk code using the derive: message

import std/[unittest, tables, strutils]
import ../src/nimtalk/core/types
import ../src/nimtalk/interpreter/evaluator
import ../src/nimtalk/interpreter/objects
import ../src/nimtalk/parser/parser

suite "derive: from Nimtalk code":
  setup:
    var interp = newInterpreter()
    initGlobals(interp)

  test "can call derive: from Nimtalk code":
    # Create a Person class with instance variables
    let source = """
Person := Object derive: #(name age).
"""
    let (result, err) = interp.doit(source)

    if err.len > 0:
      echo "Error: ", err

    check err.len == 0
    check result.kind == vkObject

    let personClass = result.objVal
    check personClass.hasSlots == true

    # Check that the slots were created
    let slotNames = personClass.getSlotNames()
    check slotNames.len == 2
    check "name" in slotNames
    check "age" in slotNames

  test "can use automatic accessors from Nimtalk code":
    # Create class and instance, then use accessors
    let source = """
Person := Object derive: #(name age).
person := Person derive.
person name: "Alice".
person age: 30.
"""
    let (result, err) = interp.doit(source)

    if err.len > 0:
      echo "Error: ", err

    check err.len == 0

    # Verify the person instance was created
    check interp.globals.hasKey("person")
    let personVal = interp.globals["person"]
    check personVal.kind == vkObject

    let person = personVal.objVal
    check person.hasSlots == true

    # Check that values were set
    let nameVal = person.getSlot("name")
    check nameVal.kind == vkString
    check nameVal.strVal == "Alice"

    let ageVal = person.getSlot("age")
    check ageVal.kind == vkInt
    check ageVal.intVal == 30

  test "inheritance of ivars works from Nimtalk code":
    # Create Animal with species, then Dog with breed
    let source = """
Animal := Object derive: #(species).
Dog := Animal derive: #(breed).
dog := Dog derive.
dog species: "Canine".
dog breed: "Golden Retriever".
"""
    let (result, err) = interp.doit(source)

    if err.len > 0:
      echo "Error: ", err

    check err.len == 0

    # Verify the dog instance
    check interp.globals.hasKey("dog")
    let dogVal = interp.globals["dog"]
    check dogVal.kind == vkObject

    let dog = dogVal.objVal
    check dog.hasSlots == true

    # Should have both inherited and own ivars
    let slotNames = dog.getSlotNames()
    check slotNames.len == 2
    check "species" in slotNames
    check "breed" in slotNames

    # Check inherited ivar value
    let speciesVal = dog.getSlot("species")
    check speciesVal.kind == vkString
    check speciesVal.strVal == "Canine"

    # Check own ivar value
    let breedVal = dog.getSlot("breed")
    check breedVal.kind == vkString
    check breedVal.strVal == "Golden Retriever"

  test "detects duplicate ivar names":
    # Try to create class with duplicate ivar (should error)
    let source = """
Animal := Object derive: #(species).
Dog := Animal derive: #(species breed).
"""
    let (result, err) = interp.doit(source)

    check err.len > 0
    let conflictFound = err.find("Instance variable conflict") >= 0
    check conflictFound

  test "empty ivar array creates object without slots":
    # Create class with no ivars
    let source = """
Simple := Object derive: #().
simple := Simple derive.
"""
    let (result, err) = interp.doit(source)

    if err.len > 0:
      echo "Error: ", err

    check err.len == 0

    let simpleVal = interp.globals.getOrDefault("simple")
    check simpleVal.kind == vkObject

    let simple = simpleVal.objVal
    # Should not have slots
    check simple.hasSlots == false or simple.slots.len == 0
