# Simple Nimtalk Slot-based Ivars Benchmark
# Compares slot access vs property bag access

import std/[times, tables]
import ../src/nimtalk/core/types
import ../src/nimtalk/interpreter/objects

echo "Nimtalk Slot-based Ivars Performance"
echo "===================================="
echo ""

# Test direct slot access (fastest)
echo "Testing direct slot access..."
let slot_iterations = 100000
let ivars = @["a", "b", "c", "d", "e"]
var obj1 = initSlotObject(ivars)

for i in 0..<5:
  obj1.slots[i] = NodeValue(kind: vkInt, intVal: i)

let start1 = cpuTime()
for i in 0..<slot_iterations:
  let idx = i mod 5
  discard obj1.slots[idx]
let end1 = cpuTime()

echo "  Iterations: " & $slot_iterations
echo "  Time (ms): " & $((end1 - start1) * 1000)
echo ""

# Test named slot access (medium)
echo "Testing named slot access..."
var obj2 = initSlotObject(ivars)

for i in 0..<5:
  obj2.setSlot(ivars[i], NodeValue(kind: vkInt, intVal: i))

let start2 = cpuTime()
for i in 0..<slot_iterations:
  let idx = i mod 5
  discard obj2.getSlot(ivars[idx])
let end2 = cpuTime()

echo "  Iterations: " & $slot_iterations
echo "  Time (ms): " & $((end2 - start2) * 1000)
echo ""

# Test property bag access (slowest)
echo "Testing property bag access..."
var root = initRootObject()
var obj3 = deriveImpl(root, @[]).objVal

# Add 5 properties
for i in 0..<5:
  let key = "prop" & $i
  obj3.properties[key] = NodeValue(kind: vkInt, intVal: i)

let start3 = cpuTime()
for i in 0..<slot_iterations:
  let idx = i mod 5
  let key = "prop" & $idx
  discard obj3.properties[key]
let end3 = cpuTime()

echo "  Iterations: " & $slot_iterations
echo "  Time (ms): " & $((end3 - start3) * 1000)
echo ""

echo "Summary:"
echo "--------"
echo "Direct slots: " & $((end1 - start1) * 1000) & " ms"
echo "Named slots:  " & $((end2 - start2) * 1000) & " ms" 
echo "Property bag: " & $((end3 - start3) * 1000) & " ms"
echo ""
echo "Slot-based access is " & $((end3 - start3) / (end1 - start1)) & "x faster than property bags!"
