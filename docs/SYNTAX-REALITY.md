# Nimtalk Syntax - Current Implementation

This document describes the **actual implemented syntax** in the current Nimtalk interpreter, which may differ from proposal documents.

## String Literals

**Both single and double quotes work** for string literals:

```smalltalk
"This is a string"
'This is also a string'
```

The lexer (`src/nimtalk/parser/lexer.nim`) accepts both `'` and `"` as valid string delimiters. Use whichever style you prefer.

## Comments

Nimtalk uses **Smalltalk-style double-quoted comments**:

```smalltalk
"This is a comment"

Object at: 'clone' put: [
  "Create a shallow copy of this object"
  ^ self perform: 'primitiveClone'
].
```

**Not hash-comments**: The `#` character has special meanings in Nimtalk:
- `#(1 2 3)` - Array literal
- `#{key: value}` - Table literal
- `#symbol` - Symbol literal
- `# comment` - Only works when `#` is followed by whitespace

**Important**: The comment syntax `"comment"` (with double quotes) is distinct from string literals `"string"` by context. In practice, comments appear where statements would be, while strings appear as values.

## Block Syntax

### Basic Blocks

Blocks are enclosed in square brackets `[]`:

```smalltalk
# Block without arguments
[ 1 + 2 ]

# Block with one argument
[ :x | x * 2 ]

# Block with multiple arguments
[ :x :y | x + y ]
```

### The `|` Separator

The vertical bar `|` **only appears when you have block parameters or temporaries**:

```smalltalk
# WITHOUT parameters or temporaries - no | needed
[ 1 + 2 ]

# WITH parameters but no temporaries - single | separates params from code
[ :x | x * 2 ]

[ :x :y | x + y ]

# WITHOUT params but WITH temporaries - | | encloses temporaries
[ | temp1 temp2 |
  temp1 := 1.
  temp2 := 2
]

# WITH parameters AND temporaries - two | separators
[ :block | index size |
  index := 0.
  size := self size
]
```

**Nimtalk-specific feature**: Unlike most Smalltalk implementations, Nimtalk allows you to omit the `|` when a block has parameters but no temporaries. This is more concise while remaining unambiguous:

```smalltalk
# Nimtalk - valid and concise
Object at: 'error:' put: [
  :message
  self error: 'Message not understood: ' + message asString
].

# Traditional Smalltalk would require:
# Object at: 'error:' put: [
#   :message |
#   self error: 'Message not understood: ' + message asString
# ].
```

**Pattern**: `[:param1 :param2 | temporary1 temporary2 | code...]`

When there's no second `|` block (temporaries), the first `|` acts as the separator between parameters and code.

### Block Parameters and Temporaries

- **Parameters**: Start with colon `:`, listed before any `|`
- **Temporaries**: Listed between `|` and `|`, separated by spaces
- **Code**: After the final `|`

**Complete pattern examples:**

```smalltalk
# No params, no temps
[ code ]

# Params, no temps (Nimtalk style - optional |)
[ :x :y | code ]

# Params, no temps (explicit - also valid)
[ :x :y | | code ]

# No params, temps
[ | temp1 temp2 | code ]

# Params and temps
[ :x :y | temp1 temp2 | code ]
```

## Method Definition Syntax

### Current Syntax (Implemented)

Methods are defined using explicit message passing:

```smalltalk
# Define a method on Object
Object at: 'printString' put: [
  ^ '<' + (self className) + '>'
].

# Keyword method
Array at: 'inject:into:' put: [
  :initialValue
  :block
  | result |
  result := initialValue.
  self do: [ :each |
    result := block value: result value: each
  ].
  ^ result
].
```

### Planned Syntax (Not Yet Implemented)

The `>>` syntax described in proposal documents is **planned but not yet implemented**:

```smalltalk
# This syntax does NOT work yet
Person>>greet [ ^ "Hello" ]

Person>>name: aName [ name := aName ]

Person>>moveX: x y: y [
  positionX := x.
  positionY := y
]
```

**TODO**: Parser extension needed to recognize `>>` and convert it to `at:put:` calls. See `TODO.md` line 38.

## Assignment

Use `:=` for assignment:

```smalltalk
obj := Object derive.
x := 5.
name := "Alice".
```

## Literals

```smalltalk
# Numbers
42
3.14159
-10

# Strings
"hello"
'world'

# Symbols
#symbol
#'symbol with spaces'

# Arrays (use Nim's seq)
#(1 2 3 "four")

# Tables (use Nim's Table)
#{key1: "value1" key2: 42}
```

## Message Passing

Nimtalk uses Smalltalk-style message passing:

```smalltalk
# Unary messages
self printString

# Binary messages (not fully implemented)
# x + y

# Keyword messages
obj at: "key" put: value
array inject: 0 into: [ :sum :each | sum + each ]
```

## Differences from Smalltalk

1. **No cascading**: The `;` operator is not implemented
2. **No binary operators**: `+`, `-`, etc. are not special syntax (yet)
3. **No metaclasses**: Class methods are defined on the class object itself
4. **Nim integration**: Can embed Nim code using `<primitive>` tags

## Embedding Nim Code

Use `<primitive>` tags for native Nim implementations:

```smalltalk
Object at: 'primitiveClone' put: <primitive>
  ## Create a shallow copy in Nim
  result = clone(self)
</primitive>.
```

## Key Files Reference

- **Lexer**: `src/nimtalk/parser/lexer.nim` - Tokenization rules
- **Parser**: `src/nimtalk/parser/parser.nim` - Syntax parsing
- **Examples**: `examples/*.nt` - Working syntax examples
- **Core objects**: `lib/core/*.nt` - Standard library implementations

---

**Note**: This reflects the syntax as of the current implementation. Check proposal documents (`docs/NIMTALK-NEW-OBJECT-MODEL.md`, etc.) for planned features and syntax changes.
