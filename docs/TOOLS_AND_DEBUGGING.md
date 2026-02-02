# Nemo Tools and Debugging Guide

## Overview

Nemo provides several command-line tools to support development, debugging, and deployment:

- `nemo` - REPL and interpreter for interactive development
- `nemoc` - Compiler for transforming Nemo to Nim code
- `build.nims` - Build automation script

## The REPL: nemo

The `nemo` command provides both interactive REPL and script execution capabilities.

### Usage

```bash
# Start interactive REPL
nemo

# Run a script file
nemo script.nt

# Evaluate an expression
nemo -e "3 + 4"

# Show AST without executing (parse only)
nemo --ast script.nt

# Run with debug logging
nemo --loglevel DEBUG script.nt

# Combine flags
nemo --ast --loglevel DEBUG -e "Object clone"
```

### Command-Line Options

**--ast**: Dump the Abstract Syntax Tree after parsing and continue execution
- Useful for understanding how code is parsed
- Shows the hierarchical structure of expressions
- Execution continues after displaying the AST

**--loglevel <level>**: Set logging verbosity
- DEBUG: Detailed tracing of execution flow
- INFO: General operation information
- WARN: Warnings about potential issues
- ERROR: Error messages only (default)

**-e "<code>":** Evaluate an expression directly
**--test:** Run built-in tests
**--version:** Show version information
**--help:** Display usage information

### Debug Logging Output

When using `--loglevel DEBUG`, nemo provides detailed execution tracing:

```
DEBUG Evaluating node: nkMessage
DEBUG Message send: +
DEBUG Message receiver: 3
DEBUG Looking up method: +
DEBUG Found method, executing
DEBUG Executing method with 1 arguments
DEBUG Pushing activation, stack depth: 1
DEBUG Evaluating node: nkLiteral
DEBUG Returning from method: 7
DEBUG Popping activation, stack depth: 0
```

This shows:
- AST node types being evaluated
- Message sends with receivers and selectors
- Method lookup and execution
- Activation stack operations
- Return values

### Interactive REPL Commands

Inside the REPL, these commands are available:

- `:help` - Show REPL help
- `:globals` - Display global variables
- `:quit` or `^D` - Exit REPL
- `:clear` - Clear the screen
- `:trace` - Toggle execution tracing

## The Compiler: nemoc

The `nemoc` command compiles Nemo source to Nim code.

### Usage

```bash
# Compile to Nim source
nemoc compile input.nt -o output.nim

# Compile and build executable
nemoc build input.nt -d build/

# Compile, build, and run
nemoc run input.nt --release

# Show AST before compiling
nemoc compile input.nt --ast

# Compile with debug logging
nemoc compile input.nt --loglevel DEBUG
```

### Commands

**compile**: Transform Nemo to Nim source code
**build**: Compile to Nim and build executable
**run**: Compile, build, and execute the result
**help**: Show usage information
**version**: Show version information

### Options

**-o, --output <file>**: Output Nim file path (compile only)
**-d, --dir <dir>**: Output directory (default: ./build)
**-r, --release**: Build with optimization flags
**--ast**: Dump AST before compiling
**--loglevel <level>**: Set logging verbosity
**-h, --help**: Show help
**-v, --version**: Show version

## Nimble Tasks

Nimble provides convenient build automation.

```bash
# Build both nemo and nemoc (binaries in subdirectories)
nimble build

# Build and copy binaries to root directory
nimble local

# Run tests
nimble test

# Clean build artifacts
nimble clean

# Install to ~/.local/bin/
nimble install
```

## Debugging Techniques

### 1. AST Inspection

Use `--ast` to understand how code is parsed:

```bash
# See AST for expression
nemo --ast -e "3 + 4"

# See AST for complex code
nemo --ast script.nt
```

AST output shows the structure:
```
Message(+)
  receiver:
    Literal(3)
  arg:
    Literal(4)
```

### 2. Execution Tracing

Use `--loglevel DEBUG` to trace execution:

```bash
# Trace message sends and method execution
nemo --loglevel DEBUG script.nt

# Combine with AST output
nemo --ast --loglevel DEBUG script.nt
```

### 3. Interactive Exploration

Use the REPL to test small pieces of code:

```bash
$ nemo
nt> obj := Object derive
nt> obj at: #test put: 42
nt> obj at: #test
42
```

### 4. Test Cases

Create test files to isolate and reproduce issues:

```smalltalk
# test_debug.nt
test := Object derive.
test at: #value put: 3 + 4.
test at: #value  "Should be 7"
```

Then run: `nemo --ast --loglevel DEBUG test_debug.nt`

## Common Issues and Solutions

### "Message not understood" Errors

This means the method doesn't exist on the receiver:

```bash
# Debug with --loglevel DEBUG
nemo --loglevel DEBUG script.nt

# Check what the receiver actually is
nemo -e "obj := Object clone. obj unknownMessage"
```

Debug output shows:
```
DEBUG Looking up method: unknownMessage
DEBUG Method not found, sending doesNotUnderstand:
```

### Parse Errors

Use `--ast` to see if code is parsed correctly:

```bash
# See parse error details
nemo --ast script_with_error.nt

# Compare with working code
nemo --ast working_script.nt
```

### Execution Differences

If interpreter and compiler behave differently:

```bash
# Test with interpreter
nemo --loglevel DEBUG script.nt

# Test AST (same for both)
nemo --ast script.nt
nemoc compile script.nt --ast
```

## Best Practices

1. **Start with AST**: Use `--ast` to verify parsing before debugging execution
2. **Use DEBUG logging**: Add `--loglevel DEBUG` when behavior is unexpected
3. **Test incrementally**: Build test cases that isolate specific features
4. **Use REPL for exploration**: Test expressions interactively before writing scripts
5. **Check return values**: Use `-e` to test small expressions quickly

## Integration with Development Workflow

### Editor Integration

- Configure your editor to run `nemo --ast` on current file
- Set up keyboard shortcuts for quick REPL access
- Use `--loglevel DEBUG` in build scripts for CI debugging

### Testing

```bash
# Run all tests with debug logging
nim e build.nims test --loglevel DEBUG

# Test specific file with AST output
nemo --ast tests/test_specific.nim
```

### Continuous Integration

```bash
# Example CI script
#!/bin/bash
set -e

# Verify AST parses correctly
nemo --ast examples/*.nt

# Run all tests with info logging
nim e build.nims test --loglevel INFO

# Run any example scripts
nemo examples/demo.nt
```

## Advanced Debugging

### Adding Custom Debug Logging

When developing Nemo itself, add debug statements:

```nim
import std/logging

# In evaluator or other components
debug("Custom trace: ", someValue)
```

These only appear with `--loglevel DEBUG`.

### Programmatic Logging Control

For tests and embedded usage, control logging programmatically:

```nim
import std/logging
import nimtalk/core/types

# Configure logging with specific level
configureLogging(lvlError)  # Suppress debug output
configureLogging(lvlDebug)  # Enable debug output

# Or just set the level
setLogLevel(lvlWarn)
```

Available levels: `lvlDebug`, `lvlInfo`, `lvlWarn`, `lvlError`, `lvlFatal`

This is used in the test suite to keep test output clean while allowing debug output when needed.

### Debugging the Debugger

If debugging tools themselves have issues:

```bash
# Check tool versions
nemo --version
nemoc --version

# Verify installation
which nemo
which nemoc

# Test minimal case
nemo -e "42"
```

### Performance Profiling

```bash
# Use Nim's profiler with compiled code
nemoc compile script.nt -o profile_me.nim
nim c -r -d:release --profiler:on --stackTrace:on profile_me.nim
```

## Summary

Nemo provides comprehensive debugging tools:
- `--ast` for parsing inspection
- `--loglevel DEBUG` for execution tracing
- REPL for interactive exploration
- `build.nims` for automation

Combine these tools to quickly identify and resolve issues during development.
