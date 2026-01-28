# Nimtalk Language Specification

Nimtalk is a prototype-based Smalltalk dialect that compiles to Nim code. This document specifies the complete language syntax, semantics, and behavior.

## Overview

Nimtalk combines Smalltalk's message-passing semantics with Nim's compilation and performance characteristics. Key features include:

- Prototype-based object system with `Object` and `Dictionary` prototypes
- Message-passing semantics (unary, binary, keyword)
- Block closures with lexical scoping
- Direct slot access for declared instance variables (149x faster than property bags)
- Method definition syntax using `>>` operator
- `self` and `super` keywords for method dispatch
- Symbol canonicalization for identity-based method lookup

## Lexical Structure

### Literals

```smalltalk
42              # Integer literal
3.14            # Float literal
"hello"         # String literal (double quotes)
'hello'         # String literal (single quotes - also accepted)
'c'             # Character literal
'\n'            # Escape sequences
```

### Symbols

```smalltalk
#symbol         # Symbol literal
#'with spaces'  # Symbol with spaces
```

### Comments

```smalltalk
# This is a comment to end of line

"This is also a comment"
```

### Arrays and Tables

```smalltalk
#(1 2 3)                    # Array literal (ordered collection)
#{"key" -> "value"}        # Table literal (key-value pairs)
```

### Identifiers

```smalltalk
variable         # Lowercase for instances
VariableName     # Capitalized for prototypes (convention)
_at:put:_        # Keyword method selector with colons
```

## Operators

### Binary Operators

All binary operators are implemented as message sends:

```smalltalk
3 + 4            # Addition
5 - 3            # Subtraction
x * y            # Multiplication
a / b            # Division
x > y            # Greater than
x < y            # Less than
x = y            # Equality
x ~= y           # Inequality
"a" , "b"        # String concatenation (comma operator)
```

### Cascade Operator

The `;` operator sends multiple messages to the same receiver:

```smalltalk
obj at: #x put: 0; at: #y put: 0; at: #z put: 0.
```

### Return Operator

The `^` operator returns a value from a block or method:

```smalltalk
[obj someMessage ^ result]
```

## Types

### ProtoObject

The root object type with declared instance variables (slots):

- `methods: Table[Symbol, BlockNode]` - Method dictionary
- `slots: seq[Value]` - For declared instance variables
- `slotNames: Table[string, int]` - Maps names to slot indices
- `parents: seq[ProtoObject]` - Prototype chain

### DictionaryObj

Extends `ProtoObject` with a property bag:

- `properties: Table[Value, Value]` - For dynamic properties

### Basic Types

- `Integer` - Signed integers
- `Float` - Floating-point numbers
- `String` - Text strings
- `Boolean` - `true` or `false`
- `Symbol` - Canonicalized identifiers
- `Nil` - Absence of value

## Object System

### Prototype Creation

```smalltalk
# Empty prototype (no property bag)
obj := Object derive

# Prototype with declared instance variables
Point := Object derive: #(x y)

# Property bag dictionary
dict := Dictionary derive
```

### Instance Variable Declaration

```smalltalk
# Declare with symbol literal
Person := Object derive: #(name age address)

# Access via automatically generated accessors
person := Person derive.
person name: "Alice".       # Generated setter
result := person name       # Generated getter
```

### Inheritance

```smalltalk
# Single inheritance
Employee := Person derive: #(salary department)

# Multi-level inheritance
Manager := Employee derive: #(teamSize)
```

## Methods

### Method Definition (>> Syntax)

```smalltalk
# Unary method
Person>>greet [ ^ "Hello, " + name ]

# Method with one parameter
Person>>name: aName [ name := aName ]

# Method with multiple keyword parameters
Point>>moveX: dx y: dy [
  x := x + dx.
  y := y + dy.
  ^ self
]

# Multiple methods in sequence
Person>>id [ ^ 42 ].
Person>>label: text [ text ].
```

The `>>` syntax is parsed and transformed into standard `at:put:` message sends.

### Method Definition (Standard Syntax)

```smalltalk
Person at: #greet put: [ ^ "Hello, " + name ]
Person at: #name: put: [ :aName | name := aName ]
```

### Method Dispatch

Method lookup proceeds through the prototype chain:

1. Check receiver's method dictionary
2. Check parent's method dictionary
3. Continue up the chain
4. Send `doesNotUnderstand:` if not found

### self and super

```smalltalk
Person>>greet [
  ^ "Hello, I'm " + name    # self is implicit for instance variables
]

Employee>>greet [
  ^ super greet + " from " + department  # Calls parent's greet
]
```

- `self` - The receiver of the message (dynamic dispatch from receiver)
- `super` - The parent of the object where the method was defined

## Blocks

### Block Syntax

```smalltalk
# No parameters
[ 1 + 2 ]

# One or more parameters
[ :x | x * 2 ]

# Multiple parameters
[ :x :y | x + y ]

# Temporaries (variables local to the block)
[ :block | index size |
  index := 0.
  size := self size
]

# Parameters and temporaries
[ :x :y | temp1 temp2 | code ]
```

### Lexical Scoping

Blocks capture variables from their enclosing scope:

```smalltalk
value := 10
block := [ value + 1 ]  # Captures 'value'
block value             # Returns 11
```

## Control Flow

Conditional execution via message sends:

```smalltalk
# If-then-else
(x > 0) ifTrue: [ "positive" ] ifFalse: [ "negative" ]

# While loop
[ x < 10 ] whileTrue: [ x := x + 1 ]

# Until loop
[ x >= 10 ] whileFalse: [ x := x + 1 ]
```

## Assignment

```smalltalk
x := 42
name := "Alice"
obj := Object derive
```

## Built-in Messages

### Object

- `derive` - Create a new prototype
- `derive: #(ivar1 ivar2)` - Create with declared instance variables
- `at: "key"` - Get property
- `at: "key" put: value` - Set property (only on Dictionary)

### Boolean

- `ifTrue: [block]` - Execute block if true
- `ifFalse: [block]` - Execute block if false
- `ifTrue: [a] ifFalse: [b]` - Conditional

### Collection Messages

- `at: index` - Get element
- `do: [ :each | ... ]` - Iterate
- `select: [ :each | ... ]` - Filter
- `collect: [ :each | ... ]` - Transform

## Execution Models

### Interpreter

AST-based interpreter for REPL and development:
- Parses source to Abstract Syntax Tree
- Evaluates AST nodes dynamically
- Provides live programming experience

### Compiler

Translates Nimtalk to Nim source:
- Generates Nim procedures for methods
- Inline optimization for known operations
- Compiles to native binaries

## Tooling

### Command-Line

```bash
ntalk                  # Interactive REPL
ntalk script.nt        # Run script
ntalk -e "3 + 4"       # Evaluate expression
ntalk --ast script.nt  # Show AST
ntalk --loglevel DEBUG # Debug output
```

### Logging

Both `ntalk` and `ntalkc` support `--loglevel`:
- `DEBUG` - Detailed execution trace
- `INFO` - General information
- `WARN` - Warnings only
- `ERROR` - Errors only (default)

## Performance

### Instance Variable Access

| Method | Performance |
|--------|-------------|
| Direct slot access | 0.8ms per 100k ops |
| Named slot access | 67ms per 100k ops |
| Property bag access | 119ms per 100k ops |

Slot-based access is **149x faster** than property bag access.

## Related Documentation

- `SYNTAX-QUICKREF-updated.md` - Syntax quick reference
- `NIMTALK-NEW-OBJECT-MODEL.md` - Object model design
- `IMPLEMENTATION-PLAN.md` - Implementation roadmap
- `TOOLS_AND_DEBUGGING.md` - Debugging guide

*Last updated: 2026-01-28 (Complete specification of implemented language features)*
