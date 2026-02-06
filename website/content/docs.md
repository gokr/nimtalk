---
title: Documentation
---

## Getting Started

### Installation

```bash
# Clone the repository
git clone https://github.com/gokr/harding.git
cd harding

# Build and install locally
nimble local

# Or install to ~/.local/bin
nimble install
```

Requirements:
- Nim 2.0 or later
- nimble (comes with Nim)

### Quick Start

```bash
# Interactive REPL
harding

# Run a script
harding script.hrd

# Evaluate an expression
harding -e "3 + 4"

# Show AST then execute
harding --ast script.hrd

# Debug output
harding --loglevel DEBUG script.hrd
```

## Learning Harding

### For Smalltalk Programmers

Start here if you know Smalltalk:

- **Key Differences** - [see below](#differences-from-smalltalk)
- [Smalltalk Compatibility Guide](/docs/MANUAL.md#smalltalk-compatibility)
- [Syntax Quick Reference](/docs/QUICKREF.md)

### For Newcomers

New to Smalltalk? Harding is a great way to learn:

1. Start with the [Language Manual](/docs/MANUAL.md)
2. Try the [Examples](/examples/)
3. Read the [Quick Reference](/docs/QUICKREF.md)

### Example Code

Hello World:
```harding
"Hello, World!" println
```

Factorial:
```harding
factorial := [:n |
    (n <= 1) ifTrue: [^ 1].
    ^ n * (factorial value: (n - 1))
]

(factorial value: 5) println   # 120
```

Counter Class:
```harding
Counter := Object derive: #(count)

Counter extend: [
    self >> initialize [
        count := 0
    ]

    self >> increment [
        count := count + 1.
        ^ count
    ]

    self >> value [
        ^ count
    ]
]

c := Counter new
c initialize
c increment
c value println   # 1
```

## Reference Documentation

### Language Reference

| Document | Description |
|----------|-------------|
| [Language Manual](/docs/MANUAL.md) | Complete language specification |
| [Quick Reference](/docs/QUICKREF.md) | Syntax cheat sheet |
| [Implementation Notes](/docs/IMPLEMENTATION.md) | VM internals |

### Tools and Development

| Document | Description |
|----------|-------------|
| [Tools & Debugging](/docs/TOOLS_AND_DEBUGGING.md) | CLI usage, debugging |
| [VSCode Extension](/docs/VSCODE.md) | Editor support |
| [GTK Integration](/docs/GTK.md) | GUI development |

### Project

| Document | Description |
|----------|-------------|
| [Future Plans](/docs/FUTURE.md) | Roadmap |
| [Contributing](/CLAUDE.md) | Development guidelines |

## Differences from Smalltalk

### Syntax Changes

| Feature | Smalltalk | Harding |
|---------|-----------|---------|
| Comments | `"comment"` | `# comment` |
| Strings | `'string'` | `"string"` |
| Statement separator | Period `.` | Period or newline |
| Class creation | Class definition | `Object derive: #(vars)` |

### Semantic Changes

**No Metaclasses**
In Harding, classes are objects but there are no metaclasses:

```harding
# Instance method
Person >> greet [ ^ "Hello" ]

# Class method - just a method on the class object
Person class >> newPerson [ ^ self new ]
```

**Multiple Inheritance**
Harding supports multiple inheritance with conflict detection.

**nil as Object**
`nil` is an instance of `UndefinedObject`:

```harding
nil class     # UndefinedObject
nil isNil     # true
```

## VSCode Extension

Syntax highlighting for `.hrd` files:

```bash
code --install-extension harding-lang-0.1.0.vsix
```

Features:
- Syntax highlighting
- Comment toggling
- Bracket matching

## Environment Variables

- `HARDING_HOME` - Default directory for loading libraries

## Getting Help

- [GitHub Issues](https://github.com/gokr/harding/issues) - Bug reports
- [GitHub Discussions](https://github.com/gokr/harding/discussions) - Questions and ideas

## Contributing

See [CLAUDE.md](/CLAUDE.md) for:
- Code style guidelines
- Build instructions
- Architecture overview
