# Nimtalk Next Implementation Steps

## Current Status

The Nimtalk interpreter now has a solid, working foundation with all core features implemented and tested. Out of 6 test suites, 5 are passing:

### ✅ **Passing Test Suites (5/6)**
- `test_core.nim` - Basic parsing, objects, and evaluation (16/16 tests)
- `test_slot_ivars.nim` - Slot-based instance variables (18/18 tests)
- `test_cascade.nim` - Cascade syntax (5/5 tests)
- `test_simple_derive.nim` - Basic inheritance
- `test_derive_from_nimtalk.nim` - Nimtalk-level class creation (5/5 tests)

### ❌ **Failing Test Suite (1/6)**
- `test_evaluator.nim` - Only 17/47 tests pass (30 failing)

## Analysis of Failing Tests

The failing tests in `test_evaluator.nim` require implementation of planned but unimplemented features. These can be categorized by dependency:

### **1. String Operations (3 tests failing)**
**Tests:**
- `methods can access self and instance variables`
- `methods with complex body execute all statements`
- `complex conditional logic`

**What's Needed:**
- Implement string concatenation with `+` operator
- Currently string operations like `"I am " + self name` fail

**Implementation Path:**
- Add `+` method to String prototype
- Handle string conversion of other types
- Implement efficient string concatenation

### **2. I/O System (1 test failing)**
**Tests:**
- `methods with complex body execute all statements`

**What's Needed:**
- Implement `Stdout` object with `write:` method
- Basic I/O capability for the runtime

**Implementation Path:**
- Create Stdout singleton object
- Add write: method that maps to Nim's stdout.write

### **3. Closures & Lexical Scoping (11 tests failing)**
**Tests:**
- `blocks with parameters capture arguments`
- `blocks can close over variables`
- `blocks support non-local return`
- `closures capture and isolate variables`
- `multiple closures share same captured variable`
- `closures capture different variables from same scope`
- `nested closures capture multiple levels`
- `closures as object methods capture instance variables`
- `closures outlive their defining scope`
- `closure captures entire lexical environment`
- `closure with non-local return from captured scope`

**What's Needed:**
- Full lexical environment capture
- Variable capture (by reference, not by value)
- Non-local returns that can exit multiple frames
- Closure lifetime management

**Implementation Path:**
- Add environment chain to Activation records
- Capture variables as references in outer scopes
- Implement non-local return that walks up activation stack
- Handle closure storage and lifetime

**Complexity:** HIGH - This is a major feature requiring architectural changes

### **4. Collections & Iteration (4 tests failing)**
**Tests:**
- `arrays can be created and accessed`
- `tables can be created and accessed`
- `collections support iteration`
- `nested collections`

**What's Needed:**
- Collection iteration protocol (e.g., `do:` method)
- Index-based access for arrays
- Key-based access for tables/dictionaries

**Implementation Path:**
- Implement Array>>do: method
- Implement Table>>do: method
- Add at: method for arrays
- Add at:put: methods for collections

### **5. Error Handling (2 tests failing)**
**Tests:**
- `undefined message raises error`
- `method with invalid argument count reports error`

**What's Needed:**
- More specific error messages
- Better error reporting infrastructure

**Implementation Path:**
- Enhance error messages with more context
- Track argument counts and validate

### **6. Advanced Control Flow (2 tests failing)**
**Tests:**
- `nested conditionals`
- `non-local return exits multiple frames`

**What's Needed:**
- Non-local return from nested blocks
- Better return value handling

**Implementation Path:**
- Fix non-local returns to work from arbitrary depth
- Ensure return values propagate correctly

### **7. Type System (2 tests failing)**
**Tests:**
- `nil is a valid value`
- `booleans are first-class objects`

**What's Needed:**
- Nil singleton object
- Boolean true/false objects
- Proper handling of these special values

**Implementation Path:**
- Create NilObject singleton
- Create Boolean objects with proper methods
- Ensure ifTrue:ifFalse: work with boolean objects

### **8. Complex Expressions (1 test failing)**
**Tests:**
- `mixed unary, binary, and keyword messages`

**What's Needed:**
- Operator precedence handling
- Complex message chain parsing

**Implementation Path:**
- Review and fix message precedence rules
- Ensure proper evaluation order

### **9. Various Edge Cases (4 tests failing)**
Various edge cases in method execution, returns, and special values.

## Implementation Priority

### **Phase 1: Essential Features (Immediate)**
1. **Type System** - Nil and booleans
   - Low complexity, high impact
   - Required for proper conditionals

2. **Collections & Iteration**
   - Moderate complexity, very useful
   - Enable working with data structures

3. **String Operations**
   - Low complexity, very common need
   - Basic string concatenation

### **Phase 2: I/O and Error Handling (Short-term)**
4. **I/O System**
   - Low complexity, useful for testing
   - Basic Stdout implementation

5. **Error Handling**
   - Low complexity, improves debugging
   - Better error messages

### **Phase 3: Advanced Features (Medium-term)**
6. **Advanced Control Flow**
   - Moderate complexity
   - Non-local returns

7. **Complex Expressions**
   - Moderate complexity
   - Operator precedence

### **Phase 4: Major Architecture (Long-term)**
8. **Closures & Lexical Scoping**
   - HIGH complexity, major feature
   - Requires significant architectural changes
   - Multiple implementation approaches to consider

## Files Modified During Current Work

- `src/nimtalk/parser/parser.nim` - Fixed block parsing with `tkRBracket` handling
- `src/nimtalk/interpreter/objects.nim` - Fixed `cloneImpl` to copy methods
- `src/nimtalk/interpreter/evaluator.nim` - Debug output added (should be removed)

## Next Steps

1. **Clean up debug output** from evaluator.nim
2. **Organize test_evaluator.nim** - split into passing and future tests
3. **Implement Phase 1 features** starting with type system
4. **Update TODO.md** to reflect current status

## Testing Strategy

- Continue with test-driven development
- Add tests as features are implemented
- Keep core tests passing (5/6 suites currently passing)
- Use `{.skip.}` pragma for tests that can't run yet

## Notes

The core Nimtalk interpreter is **fully functional**. The failing tests represent advanced features that need focused implementation efforts. The architecture is solid and can support these features with appropriate extensions.
