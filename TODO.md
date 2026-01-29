# Nimtalk Development TODO

This document tracks current work items and future directions for Nimtalk development.

## Current Status

**Core Language**: The interpreter is functional with:
- Lexer, parser, AST interpreter
- Prototype object system with slots and property bags
- REPL with file execution
- Block closures with lexical scoping and non-local returns
- Method definition syntax (`>>`)
- `self` and `super` support
- Standard library (collections, core objects)

**Not Yet Implemented**: Compiler (ntalkc is currently a stub), FFI, advanced standard library.

## High Priority

### Compiler
- [ ] Method compilation from AST to Nim procedures
- [ ] Nim type definitions for ProtoObject
- [ ] Symbol export for compiled methods
- [ ] Working `ntalkc` (currently stub)

### FFI Integration
- [ ] Nim type marshaling
- [ ] FFI bridge for calling Nim functions
- [ ] Nim module imports
- [ ] Type conversion utilities

## Medium Priority

### Standard Library
- [ ] Number objects with full arithmetic
- [ ] String manipulation methods
- [ ] Boolean conditional logic
- [ ] Enhanced collection methods

### Performance
- [ ] Method caching
- [ ] AST optimization passes
- [ ] Memory management improvements

### Tooling
- [ ] REPL history and completion
- [ ] Editor syntax highlighting definitions
- [ ] Build system refinements

## Future Directions

### BitBarrel Integration
- [ ] First-class barrel objects
- [ ] Transparent persistence
- [ ] Crash recovery support

### Language Evolution
- [ ] Optional static type checking
- [ ] Module/namespace system
- [ ] Concurrency model
- [ ] Metaprogramming APIs

## Known Issues

- Parser edge cases with nested blocks
- Memory management for circular references
- Error handling improvements needed
- Test coverage gaps in edge cases

## Documentation Needs

- [ ] Tutorials and comprehensive examples
- [ ] API reference for built-in objects
- [ ] Internal API documentation
- [ ] Help text improvements

## Build Quick Reference

```bash
nimble build       # Build ntalk and ntalkc
nimble local       # Build and copy to root
nimble test        # Run tests
nimble clean       # Clean artifacts
```

---

*Last Updated: 2026-01-29*
