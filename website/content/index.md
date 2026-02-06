---
title: Harding Smalltalk
tagline: Smalltalk Semantics, Nim Performance
---

## Hero Section

**Title:** Harding Smalltalk
**Subtitle:** Smalltalk semantics. Nim performance.
**Description:** A modern Smalltalk dialect that compiles to native code. File-based, git-friendly, and designed for the modern era.

**CTA Primary:** Get Started
**CTA Secondary:** See Examples

### Hero Code Example

```harding
# Define a Point class with x and y slots
Point := Object derive: #(x y)

# Add methods using the >> syntax
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

# Create and use a Point
p := Point new
p x: 100 y: 200
p distanceFromOrigin println
```

## Features

### Smalltalk Heritage
Everything you love about Smalltalk - message passing, blocks, live programming - preserved and modernized.

### Native Performance
Compiles through Nim to C to machine code. No VM, no bytecode, just fast native binaries.

### File-Based
No image files. Source lives in .hrd files you can version control, diff, and edit with any editor.

### Multiple Inheritance
Full support for multiple inheritance with conflict detection. Compose behaviors, don't inherit monoliths.

### Green Threads
Cooperative multitasking with first-class Process objects. Built-in scheduler with round-robin execution.

### Nim Interop
Call Nim code directly. Access the entire Nim ecosystem: libraries, packages, and system APIs.

## Quick Start

```bash
# Clone and build
git clone https://github.com/gokr/harding.git
cd harding
nimble local

# Run
harding                    # Start REPL
harding script.hrd         # Run a file
harding -e "3 + 4"         # Evaluate expression
```

## For Smalltalkers

**What feels familiar:**
- Message syntax: unary `obj size`, binary `3 + 4`, keyword `dict at: key put: value`
- Cascade messages with `;`
- Blocks are proper lexical closures with non-local returns
- Everything is an object, everything happens via message sends
- Collection messages: `do:`, `select:`, `collect:`, `inject:into:`

**What's different:**
- Optional periods - newlines also separate statements
- Hash `#` for comments (not double quotes)
- Double quotes for strings (not single quotes)
- Class creation: `Point := Object derive: #(x y)`
- File-based, git-friendly source
