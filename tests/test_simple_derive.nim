# Quick test of deriveWithIVars: from Nimtalk

import std/tables
import ../src/nimtalk/core/types
import ../src/nimtalk/interpreter/evaluator
import ../src/nimtalk/interpreter/objects

# Create interpreter and init globals
var interp = newInterpreter()
initGlobals(interp)

# Test: Create class with instance variables from Nimtalk code
let source = "Person := Object deriveWithIVars: #(name age)."
let (result, err) = interp.doit(source)

if err.len > 0:
  echo "Error: ", err
  quit(1)

echo "Success! Created Person class with slots"
echo "Result kind: ", result.kind

# Verify the class was created and stored in globals
if interp.globals.hasKey("Person"):
  let personClass = interp.globals["Person"]
  if personClass.kind == vkObject:
    let pc = personClass.objVal
    echo "Person class has ", pc.slots.len, " slots"
    echo "Slot names: ", pc.getSlotNames()
  else:
    echo "Person is not an object"
else:
  echo "Person not found in globals"
