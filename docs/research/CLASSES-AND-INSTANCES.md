# Nemo Classes and Instances - Design Document

## Core Concept

In Nemo, we maintain **Smalltalk-style conventions** where:
- **Classes** are prototypes that start with capital letters and serve as templates
- **Instances** are objects created from classes that represent actual data/entities
- **The distinction is purely conventional** - technically everything is a prototype

This gives us the clarity of traditional OOP while keeping the flexibility of prototypes.

## Terminology

### "Classes" (Prototypes Used as Classes)
```smalltalk
# These are "classes" - capitalized, serve as templates
Person := Object derive: #(name age)
Employee := Person derive: #(employeeID)
Company := Object derive
```

**Characteristics:**
- Start with capital letter (convention)
- Stored in global `Nemo` table
- Define structure and behavior
- Typically don't hold instance data
- Used to create instances

### "Instances" (Objects Created from Classes)
```smalltalk
# These are instances - lowercase, hold actual data
alice := Person new
charlie := Employee new
acme := Company new
```

**Characteristics:**
- Typically lowercase (convention)
- Hold instance-specific data
- Inherit behavior from their class
- Created by sending `new` to classes

## Global Storage

### Nemo Global Table
```smalltalk
# All "classes" get stored in Nemo global
Nemo := Table new.

# When we assign to a capitalized variable at top level:
Person := Object derive

# It's automatically added to Nemo
Nemo at: "Person" put: Person

# Can reference as:
Nemo Person
# or just
Person
```

**Implementation:**
```nim
# When parser sees assignment to capitalized identifier at module level:
# Person := Object derive: #(name age)
#
# It generates:
# 1. var Person = classDeriveImpl(Object, ["name", "age"])
# 2. Nemo["Person"] = Person
```

## Instance Creation

### Method 1: Direct `new`
```smalltalk
# Simplest - send new to the class
alice := Person new.
alice name: "Alice".
alice age: 30.
```

### Method 2: Custom Factory Methods (Recommended)
```smalltalk
# Define factory method as a CLASS method using class>>
Person class>>newWithName: aName age: anAge [
  ^ self new
      name: aName;
      age: anAge;
      yourself
]

# Use it to create instances
alice := Person newWithName: "Alice" age: 30.
bob := Person newWithName: "Bob" age: 25.
```

### Method 3: Standard `new` (with initialize)
```smalltalk
# Instance-side initialize (called automatically by new)
Person>>initialize [
  name := "Anonymous".
  age := 0
]

# Usage - initialize is called automatically
alice := Person new.
alice name: "Alice".
alice age: 30.
```

### Method 4: Cascading Initialization
```smalltalk
# After creation, use cascaded messages
alice := Person new
  name: "Alice";
  age: 30;
  address: "123 Main St";
  yourself.
```

## Syntactic Sugar Support

### The `>>` Syntax (Parser Sugar)

The parser can convert this cleaner syntax:
```smalltalk
Person>>greet: otherPerson [
  ^ "Hello " , otherPerson name , ", I'm " , name
]
```

Into the equivalent:
```smalltalk
Person at: "greet:" put: [ :otherPerson |
  ^ "Hello " , otherPerson name , ", I'm " , name
].
```

**Requirements for Parser:**
1. Recognize `IDENTIFIER>>IDENTIFIER pattern`
2. Accept keyword selectors: `name:arg:`
3. Parse method body in `[]`
4. **Optionally accept trailing `.`**
5. Generate `at:put:` AST node

### Trailing Period Support

```smalltalk
# Both should work (parser makes trailing . optional)
Person>>greet [ ^ "Hello" ]        # No period

Person>>greet [ ^ "Hello" ].       # With period (for familiarity)
```

## Example: Complete Class Definition

### File: src/Person.nt
```smalltalk
#!/usr/bin/env nemo
#
# Person - Represents a person
#

# Derive from Object with instance variables
Person := Object derive: #(name age address)

#----------------------------------------------------------------
# Class methods (instance creation)
#----------------------------------------------------------------
Person class>>newWithName: aName age: anAge [
  ^ self new
      name: aName;
      age: anAge;
      yourself
].

Person class>>newWithName: aName age: anAge address: anAddr [
  ^ self new
      name: aName;
      age: anAge;
      address: anAddr;
      yourself
].

#----------------------------------------------------------------
# Instance methods (on instances)
#----------------------------------------------------------------
Person at: "initialize" put: [
  "Default initialization"
  name := "Anonymous"
  age := 0
  address := nil
].

Person at: "name" put: [ ^ name ].

Person at: "name:" put: [ :aName | name := aName ].

Person at: "age" put: [ ^ age ].

Person at: "age:" put: [ :anAge | age := anAge ].

Person at: "address" put: [ ^ address ].

Person at: "address:" put: [ :anAddr | address := anAddr ].

Person at: "greet" put: [
  ^ "Hello, I'm " , name
].

Person at: "greet:" put: [ :other |
  ^ "Hello " , other name , ", I'm " , name
].

Person at: "birthday" put: [
  age := age + 1.
  ^ self
].

Person at: "description" put: [
  ^ name + " (age " + age asString + ")"
].
```

### Alternative: Using Parser Sugar
```smalltalk
#!/usr/bin/env nemo
#
# Person - Using >> syntax
#

Person := Object derive: #(name age address).

# Class methods (instance creation) - note the "class>>" syntax
Person class>>newWithName: aName age: anAge [
  ^ self new
      name: aName;
      age: anAge;
      yourself
].

# Instance methods
Person>>initialize [
  name := "Anonymous".
  age := 0.
  address := nil
].

Person>>name [ ^ name ].

Person>>name: aName [
  name := aName
].

Person>>age [ ^ age ].

Person>>age: anAge [
  age := anAge
].

Person>>greet: otherPerson [
  ^ "Hello " , otherPerson name , ", I'm " , name
].

Person>>birthday [
  age := age + 1.
  ^ self
].

Person>>description [
  ^ name , " (age " , age asString , ")"
].
```

### Usage: main.nt
```smalltalk
#!/usr/bin/env nemo
#
# Main application

# Load Person (future: will auto-register in Nemo global)
load: "src/Person.nt".

# Create instances using class method
alice := Person newWithName: "Alice" age: 30.
bob := Person newWithName: "Bob" age: 25.

# Use instances
alice greet print.                         # "Hello, I'm Alice"
alice greet: bob print.                    # "Hello Bob, I'm Alice"

# State change
alice birthday.
alice age print.                           # 31

# Description
alice description print.                   # "Alice (age 31)"
```

## Inheritance Example

### Employee extends Person
```smalltalk
#!/usr/bin/env nemo
#
# Employee extends Person

# Load parent
load: "src/Person.nt".

# Derive with additional ivars
Employee := Person derive: #(employeeID department salary).

# Class method for instance creation
Employee class>>newWithName: aName age: anAge id: anID dept: aDept salary: aSalary [
  ^ self new
      name: aName;
      age: anAge;
      employeeID: anID;
      department: aDept;
      salary: aSalary;
      yourself
].

# Override parent method
Employee>>greet [
  ^ (super perform: "greet") , " from " , department , " department"
].

# New method
Employee>>giveRaise: amount [
  salary := salary + amount.
  ^ self
].
```

## Parser Implementation Details

### Recognizing `>>` Syntax

The parser needs to:

1. **Tokenize `>>`** as a single token
2. **Parse method signature** after `>>`:
   - Unary: `IDENTIFIER` (e.g., `greet`)
   - Binary: `OPERATOR` (e.g., `+`)
   - Keyword: `IDENTIFIER:` (e.g., `name:`) or `IDENTIFIER: IDENTIFIER:` (e.g., `moveX:y:`)
3. **Accept optional trailing `.`**: `.] | ] .`
4. **Generate**: `at:put:` AST node

### Grammar

```ebnf
methodDefinition := receiver ">>" methodSelector methodBody ["."]
receiver := identifier
methodSelector := unarySelector | binarySelector | keywordSelector
unarySelector := identifier
binarySelector := operator
keywordSelector := identifier ":" { identifier ":" }
methodBody := "[" statements "]"
```

### Conversion Example

```smalltalk
# Input (file definition mode)
Person>>moveX: x y: y [
  x := x + dx
  y := y + dy
].

# Parser generates AST equivalent to:
MessageSendNode(
  receiver: Identifier("Person"),
  selector: "at:put:",
  arguments: [
    StringNode("moveX:y:"),
    BlockNode([
      AssignmentNode(
        left: Identifier("x"),
        right: MessageSendNode(
          receiver: Identifier("x"),
          selector: "+",
          arguments: [Identifier("dx")]
        )
      ),
      AssignmentNode(
        left: Identifier("y"),
        right: MessageSendNode(
          receiver: Identifier("y"),
          selector: "+",
          arguments: [Identifier("dy")]
        )
      )
    ])
  ]
)
```

## Class Methods vs Instance Methods

Nemo uses a **simplified class model** compared to full Smalltalk. Understanding this distinction is important:

### The Model

In Nemo, each `Class` has two separate method tables:
- **`methods` / `allMethods`** - Instance methods (sent to instances of the class)
- **`classMethods` / `allClassMethods`** - Class methods (sent to the class itself)

```smalltalk
# Define a class method (sent to Object)
Object class>>description [ ^"The root class" ]

# Define an instance method (sent to instances)
Object>>description [ ^"An instance of Object" ]

# Usage:
Object description           # Returns "The root class" (class method)
obj := Object new.
obj description              # Returns "An instance of Object" (instance method)
```

### Key Characteristics

**1. Separate Method Spaces**
- Class methods and instance methods are completely separate
- When you send a message to a class (like `Object derive:`), it looks in `allClassMethods`
- When you send a message to an instance (like `obj x: 10`), it looks in `allMethods`

**2. Methods Must Be Defined Twice for Both Contexts**
If you need a method like `printString` to work on BOTH classes AND instances, you must define it twice:

```smalltalk
# For classes
Object class>>printString [ ^"<class " , self name , ">" ]

# For instances
Object>>printString [ ^"<instance of " , self class name , ">" ]
```

**3. No Metaclass Hierarchy**
- Unlike full Smalltalk, there's no metaclass hierarchy
- All classes are instances of the same internal `Class` type
- You cannot add methods to just the `Class` class that would be inherited by all classes (but not their instances)

**4. Inheritance Works Within Each Space**
- Class methods are inherited from parent classes via `allClassMethods`
- Instance methods are inherited from parent classes via `allMethods`
- When you call `Object derive:`, the new class copies `allClassMethods` from Object

### Comparison with Full Smalltalk

| Feature | Full Smalltalk | Nemo |
|---------|---------------|---------|
| Metaclass hierarchy | Yes (each class has its own metaclass) | No (single Class type) |
| Class methods | Inherited via metaclass chain | Inherited via allClassMethods |
| Methods on both | Automatic via metaclass | Must define twice |
| Complexity | High (metaclass of metaclass...) | Low (two method tables) |

### Why This Simplified Approach?

For Nemo's goals, this simplified approach should be sufficient:

✅ **Easier to understand** - No metaclass confusion
✅ **Easier to implement** - No complex metaclass chain
✅ **Covers most use cases** - Factory methods, class-side utilities work fine
✅ **Explicit is better** - Defining methods twice makes intent clear

The full Smalltalk metaclass system is powerful but complex. Most of its benefits can be achieved with the current design at a fraction of the complexity.

## Summary

### Key Points

1. **No new parser for derivation** - `derive:` is just a message
2. **Optional `>>` syntax** - Parser sugar that converts to `at:put:`
3. **Conventional distinction** - Classes (capitalized) vs instances (lowercase)
4. **Global Nemo table** - Stores all "classes"
5. **Multiple creation patterns** - direct, factory methods, new, cascading
6. **Full keyword support** - `Person>>moveX: x y: y [...]`
7. **Optional trailing `.`** - Flexible syntax
8. **Separate class/instance methods** - Define twice if needed for both

### Benefits of This Approach

✅ **No special parsing for derivation** - Pure message passing
✅ **Flexible instance creation** - Multiple patterns supported
✅ **Familiar conventions** - Matches Smalltalk style
✅ **Optional sugar** - `>>` syntax is just convenience
✅ **Encapsulated classes** - Stored in Nemo global
✅ **Clear structure** - Class vs instance distinction
✅ **Simpler than metaclasses** - Easier to understand and implement

This gives us the best of both worlds: the flexibility of prototypes with the clarity of conventional OOP patterns!

---

**Next**: Should we update the implementation plan to reflect this approach? Or are we ready to start coding Phase 1 (adding derive: to Object)?
