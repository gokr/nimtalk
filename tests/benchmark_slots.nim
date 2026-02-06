# Harding Slot-based Instance Variables Performance Benchmark
#
# Compares performance of slot access vs property bag access
#

import std/times
import ../src/harding/core/types
import ../src/harding/interpreter/objects

proc benchmarkPropertyBag() =
  ## Benchmark property bag access (current/old way)
  echo "=== Property Bag Access ==="
  
  let iterations = 100000
  var root = initRootObject()
  var obj = deriveImpl(root, @[]).objVal
  
  # Add properties
  for i in 0..<10:
    let key = "prop" & $i
    obj.properties[key] = NodeValue(kind: vkInt, intVal: i)
  
  let start = cpuTime()
  for i in 0..<iterations:
    let propNum = i mod 10
    let key = "prop" & $propNum
    discard obj.properties[key]
  let endTime = cpuTime()
  
  let elapsed = endTime - start
  echo "Iterations: " & $iterations
  echo "Time (ms): " & $(elapsed * 1000)
  echo ""

proc benchmarkSlotAccess() =
  ## Benchmark direct slot access (new way)
  echo "=== Direct Slot Access ==="
  
  let iterations = 100000
  let ivarNames = @["ivar0", "ivar1", "ivar2", "ivar3", "ivar4"]
  var obj = initSlotObject(ivarNames)
  
  # Initialize slots
  for i in 0..<5:
    obj.slots[i] = NodeValue(kind: vkInt, intVal: i)
  
  let start = cpuTime()
  for i in 0..<iterations:
    let slotNum = i mod 5
    discard obj.slots[slotNum]
  let endTime = cpuTime()
  
  let elapsed = endTime - start
  echo "Iterations: " & $iterations
  echo "Time (ms): " & $(elapsed * 1000)
  echo ""

proc benchmarkNamedSlotAccess() =
  ## Benchmark named slot access (with name lookup)
  echo "=== Named Slot Access ==="
  
  let iterations = 100000
  let ivarNames = @["ivar0", "ivar1", "ivar2", "ivar3", "ivar4"]
  var obj = initSlotObject(ivarNames)
  
  # Initialize slots
  for i in 0..<5:
    obj.setSlot(ivarNames[i], NodeValue(kind: vkInt, intVal: i))
  
  let start = cpuTime()
  for i in 0..<iterations:
    let slotNum = i mod 5
    discard obj.getSlot(ivarNames[slotNum])
  let endTime = cpuTime()
  
  let elapsed = endTime - start
  echo "Iterations: " & $iterations
  echo "Time (ms): " & $(elapsed * 1000)
  echo ""

when isMainModule:
  echo "Harding Slot-based Ivars Benchmark"
  echo "=================================="
  echo ""
  
  benchmarkPropertyBag()
  benchmarkSlotAccess()
  benchmarkNamedSlotAccess()
  
  echo "Lower time is better"
  echo ""
  echo "Expected ordering:"
  echo "  Direct slots < Named slots < Property bag"
  echo ""
  echo "This shows the performance improvement of the new slot-based system"
