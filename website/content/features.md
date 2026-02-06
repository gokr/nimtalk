---
title: Features
---

## Language Features

### Pure Smalltalk Semantics

Harding preserves the essence of Smalltalk:

- **Everything is an object** - Numbers, strings, blocks, classes
- **Everything happens via message passing** - No function calls, only messages
- **Late binding** - Method lookup happens at message send time
- **Blocks with non-local returns** - True closures with `^` return

```harding
# Message passing
3 + 4                    # binary message
obj size                 # unary message
dict at: key put: value  # keyword message

# Blocks with non-local return
findPositive := [:arr |
    arr do: [:n |
        (n > 0) ifTrue: [^ n]   # Returns from findPositive:
    ].
    ^ nil
]
```

### Modern Syntax

Optional periods, hash comments, double-quoted strings:

```harding
# This is a comment
x := 1                  # No period needed at end of line
y := 2
z := x + y              # But periods work too if you prefer

"Double quotes for strings"
```

### Class-Based with Multiple Inheritance

Create classes dynamically with slots and methods:

```harding
# Define a class
Point := Object derive: #(x y)

# Add methods
Point extend: [
    self >> moveBy: dx and: dy [
        x := x + dx
        y := y + dy
        ^ self
    ]

    self >> distanceFromOrigin [
        ^ ((x * x) + (y * y)) sqrt
    ]
]

# Use it
p := Point new
p x: 3 y: 4
p distanceFromOrigin println   # 5.0
```

Multiple inheritance with conflict detection:

```harding
# Inherit from multiple parents
ColoredPoint := Point derive: #(color)
ColoredPoint addParent: Comparable
ColoredPoint addParent: Printable
```

## Runtime Features

### Green Threads (Processes)

Cooperative multitasking with first-class Process objects:

```harding
# Fork a new process
worker := Processor fork: [
    1 to: 10 do: [:i |
        i println
        Processor yield
    ]
]

# Process control
worker suspend
worker resume
worker terminate

# Check state
worker state    # ready, running, blocked, suspended, terminated
```

Each process has:
- Unique PID
- Name
- State tracking
- Independent execution

### Stackless VM

Each Process runs in its own stackless VM, enabling:
- Lightweight processes
- Easy serialization
- Path to true parallelism

## Development Features

### REPL

Interactive development environment:

```bash
$ harding
Harding Smalltalk REPL
Type 'exit' to quit, 'help' for commands

> 3 + 4
7

> Point := Object derive: #(x y)
Point

> p := Point new
Point instance

> p x: 10
10

> p x
10
```

### File-Based Development

Source code lives in `.hrd` files:

```bash
# Edit with any editor
vim myprogram.hrd

# Run it
harding myprogram.hrd

# Git friendly
git add myprogram.hrd
git commit -m "Add feature"
```

### VSCode Extension

Syntax highlighting and basic IDE support:

- Syntax highlighting for `.hrd` files
- Comment toggling
- Bracket matching
- Code folding

## Standard Library

### Core Classes

**Object** - The root of all classes
- `clone`, `derive`, `class`, `isNil`
- `at:`, `at:put:`, `respondsTo:`
- `perform:`, `perform:with:`

**Block** - Closures
- `value`, `value:`, `value:value:`
- `whileTrue:`, `whileFalse:`

**Boolean** - true/false
- `ifTrue:`, `ifFalse:`, `ifTrue:ifFalse:`
- `and:`, `or:`, `not`

**Number** - Integers and floats
- Arithmetic: `+`, `-`, `*`, `/`, `//`, `\`
- Comparison: `<`, `>`, `<=`, `>=`, `=`, `==`
- `to:do:`, `to:by:do:`, `timesRepeat:`

**String** - Text
- `,` (concatenation)
- `size`, `at:`, `println`

**Collections**
- **Array** - Ordered collection `#(1 2 3)`
- **Table** - Dictionary `#{"key" -> "value"}`
- Methods: `do:`, `collect:`, `select:`, `detect:`, `inject:into:`

### Collection Examples

```harding
# Array literal
numbers := #(1 2 3 4 5)

# Table literal
scores := #{"Alice" -> 95, "Bob" -> 87}

# Iteration
numbers do: [:n | n println]

# Transformation
squares := numbers collect: [:n | n * n]

# Filter
evens := numbers select: [:n | (n \ 2) = 0]

# Reduce
sum := numbers inject: 0 into: [:acc :n | acc + n]

# Find
ten := numbers detect: [:n | n = 10]  # nil if not found
```

## Compilation Pipeline

```
Harding source (.hrd)
       ↓
   Parser (AST)
       ↓
   Interpreter (now)
       ↓
   Nim source (.nim) (coming)
       ↓
   C source (.c)
       ↓
   Machine code
       ↓
   Native binary
```

## Interoperability

### Nim Integration

Call Nim code directly via primitives:

```harding
# Access Nim functions
Array>>at: index <primitive primitiveAt: index>

# With validation
Array>>at: index [
    (index < 1 or: [index > self size]) ifTrue: [
        self error: "Index out of bounds"
    ].
    ^ <primitive primitiveAt: index>
]
```

### C Library Access

Through Nim's FFI:

```harding
# Can wrap C libraries
<primitive primitiveCCall: function with: args>
```

## Performance

### Slot-Based Instance Variables

Direct slot access is 149x faster than property bag access:

```harding
# Fast - O(1) slot access
Point>>moveBy: dx and: dy [
    x := x + dx      # Direct slot
    y := y + dy
]
```

### Native Compilation (Coming)

Compilation to native code via Nim will provide:
- Fast startup
- Efficient execution
- Small binaries
- Easy deployment

## Debugging Tools

### Log Levels

```bash
harding --loglevel DEBUG script.hrd
```

Levels: DEBUG, INFO, WARN, ERROR, FATAL

### AST Output

```bash
harding --ast script.hrd
```

Shows the parsed Abstract Syntax Tree.

### Process Inspection

```harding
# List all processes
Scheduler listProcesses

# Check process state
process state
process pid
process name
```

## What's Next

See [Future Plans](/docs/FUTURE.md) for:
- Compiler to Nim
- Actor-based concurrency
- GTK GUI bindings
- AI integration hooks
