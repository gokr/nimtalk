#!/usr/bin/env python3
"""Update documentation to reflect new syntax:
- Single quotes for strings
- Double quotes for comments
- Prototype -> Class
"""

import re
import sys

def update_specification(content):
    """Update SPECIFICATION.md"""
    # Change prototype-based to class-based
    content = content.replace('Prototype-based', 'Class-based')
    content = content.replace('prototype-based', 'class-based')
    content = content.replace('prototypes', 'classes')
    content = content.replace('prototype', 'class')
    content = content.replace('Prototype', 'Class')

    # Update string literals section
    content = content.replace(
        '"hello"         # String literal (double quotes only)',
        "'hello'         # String literal (single quotes)"
    )
    # Handle the escape sequence line carefully
    content = content.replace(
        '"\\n"            # Escape sequences (\\n, \\t, \\r, \\\", \\\\)',
        "'\\n'            # Escape sequences (\\n, \\t, \\r, \\', \\\\)"
    )
    content = content.replace(
        '**Note**: Single quotes are reserved for future use.',
        '**Note**: Double quotes are used for comments.'
    )

    # Update symbol section
    content = content.replace(
        '#"with spaces"  # Symbol with spaces (double quotes)',
        "#'with spaces'  # Symbol with spaces (single quotes)"
    )

    # Update comments section - this is a major change
    content = content.replace(
        '# This is a comment to end of line',
        '"This is a comment - double quotes for comments"'
    )
    content = content.replace(
        '#====              # Section header (no space needed after #)',
        '"==== Section header'
    )
    content = content.replace(
        '#----------         # Also works with dashes',
        '"---------- Also works with dashes'
    )
    content = content.replace(
        '**Note**: `#` followed by whitespace or special characters (`=`, `-`, `*`, `/`, `.`, `|`, `&`, `@`, `!`) marks a comment. This allows section headers like `#====` to work without requiring spaces. Smalltalk-style `"comment"` syntax is not supported.',
        '**Note**: Double quotes mark comments. Hash `#` followed by whitespace or special characters (`=`, `-`, `*`, `/`, `.`, `|`, `&`, `@`, `!`) also marks a comment for section headers.'
    )

    # Update table literals
    content = content.replace(
        '#{"key" -> "value"}',
        "#{'key' -> 'value'}"
    )

    # Update identifiers section
    content = content.replace(
        'VariableName     # Capitalized for prototypes (convention)',
        'VariableName     # Capitalized for classes (convention)'
    )

    # Update string concatenation example
    content = content.replace(
        '"a" , "b"        # String concatenation (comma operator)',
        "'a' , 'b'        # String concatenation (comma operator)"
    )

    # Update method examples with strings
    content = content.replace(
        'Person>>greet [ ^ "Hello, " , name ]',
        "Person>>greet [ ^ 'Hello, ' , name ]"
    )
    content = content.replace(
        '^ "Hello, " + name',
        "^ 'Hello, ' + name"
    )
    content = content.replace(
        '^ "Hello " , name',
        "^ 'Hello ' , name"
    )
    content = content.replace(
        'name := "Alice"',
        "name := 'Alice'"
    )
    content = content.replace(
        '"Alice"',
        "'Alice'"
    )
    content = content.replace(
        '"Anonymous"',
        "'Anonymous'"
    )
    content = content.replace(
        '"positive"',
        "'positive'"
    )
    content = content.replace(
        '"negative"',
        "'negative'"
    )

    # Update derive method description
    content = content.replace(
        'Create a new prototype',
        'Create a new class'
    )
    content = content.replace(
        'Call a parent prototype method explicitly',
        'Call a parent class method explicitly'
    )

    return content

def update_syntax_reality(content):
    """Update SYNTAX-REALITY.md"""
    # Update header about string literals
    content = content.replace(
        '**Double quotes only** for string literals:',
        '**Single quotes** for string literals:'
    )
    content = content.replace(
        '"This is a string"',
        "'This is a string'"
    )
    content = content.replace(
        "Single quotes are reserved for future use (possibly character literals).",
        "Double quotes are used for comments."
    )

    # Update comments section
    content = content.replace(
        'Nimtalk uses **hash-style comments only** (like Nim):',
        'Nimtalk uses **double-quote comments** (like Smalltalk):'
    )
    content = content.replace(
        '# This is a comment to end of line',
        '"This is a comment - double quotes for comments"'
    )
    content = content.replace(
        '# Create a shallow copy of this object',
        '"Create a shallow copy of this object"'
    )
    content = content.replace(
        '# Array literal',
        '"Array literal'
    )
    content = content.replace(
        '# Table literal',
        '"Table literal'
    )
    content = content.replace(
        '# Symbol',
        '"Symbol'
    )
    content = content.replace(
        "**Note**: Smalltalk-style `\"comment\"` syntax is not supported. Use `#` for all comments.",
        "**Note**: Hash-style `# comment` syntax is also supported for section headers."
    )

    # Update string literals in examples
    content = content.replace(
        '"hello"',
        "'hello'"
    )
    content = content.replace(
        '#"symbol with spaces"',
        "#'symbol with spaces'"
    )
    content = content.replace(
        '"key1" -> "value1"',
        "'key1' -> 'value1'"
    )
    content = content.replace(
        '"key2" -> 42',
        "'key2' -> 42"
    )
    content = content.replace(
        '#symbolKey -> "value"',
        "#symbolKey -> 'value'"
    )
    content = content.replace(
        '"Hello World"',
        "'Hello World'"
    )
    content = content.replace(
        '"Hello, " + name',
        "'Hello, ' + name"
    )

    return content

def update_syntax_quickref(content):
    """Update SYNTAX-QUICKREF-updated.md"""
    # Update prototype -> class
    content = content.replace('prototype', 'class')
    content = content.replace('Prototype', 'Class')

    # Update string literals
    content = content.replace('"Alice"', "'Alice'")
    content = content.replace('"Hello, " , name', "'Hello, ' , name")
    content = content.replace('"Validate age is positive"', "'Validate age is positive'")
    content = content.replace('"Age must be positive"', "'Age must be positive'")
    content = content.replace('"Anonymous"', "'Anonymous'")
    content = content.replace('" from " , department', "' from ' , department")
    content = content.replace('"hello world"', "'hello world'")
    content = content.replace("'a'                     # Character literal",
                              "#'a'                    # Character symbol")
    content = content.replace("'\\n'                    # Newline character",
                              "#'newline'              # Newline symbol")
    content = content.replace("'\\t'                    # Tab character",
                              "#'tab'                  # Tab symbol")
    content = content.replace('"positive"', "'positive'")
    content = content.replace('"negative"', "'negative'")
    content = content.replace('"Object"', "'Object'")
    content = content.replace('"double quoted"', "'single quoted'")
    content = content.replace('"""multiline"""', "'''multiline'''")

    # Update comments - use a helper to avoid issues
    replacements = [
        ('# Declare instance variables when creating class', '"Declare instance variables when creating class'),
        ('# (derive: is a regular message, not special syntax)', '"derive: is a regular message, not special syntax"'),
        ('# Symbols in arrays need # prefix: #(#name #age)', '"Symbols in arrays need # prefix: #(#name #age)"'),
        ('# Use in .nt files for class definitions', '"Use in .nt files for class definitions'),
        ('# This syntax requires special parsing', '"This syntax requires special parsing"'),
        ('# NOT executable in REPL', '"NOT executable in REPL"'),
        ('# Property bag access (deprecated for declared classes)', '"Property bag access (deprecated for declared classes)'),
        ('# New Way (Declared Classes)', '"New Way (Declared Classes)'),
        ('# Generated accessor methods (compile to direct slot access)', '"Generated accessor methods (compile to direct slot access)"'),
        ('# Standard message sending (always available)', '"Standard message sending (always available)"'),
        ('# Direct ivar access (Inside Methods)', '"Direct ivar access (Inside Methods)'),
        ('# Direct slot access', '"Direct slot access"'),
        ('# Direct read and write', '"Direct read and write"'),
        ('# Direct ivar assignment', '"Direct ivar assignment"'),
        ('# Create then init', '"Create then init"'),
        ('# Then configure', '"Then configure"'),
        ('# Cascade messages', '"Cascade messages"'),
        ('# Call parent initialization', '"Call parent initialization"'),
        ('# Then init Employee ivars', '"Then init Employee ivars"'),
        ('"Override parent method, call super for base behavior"', "'Override parent method, call super for base behavior'"),
        ('# One class per file (recommended)', '"One class per file (recommended)'),
        ('# Defines Person class', '"Defines Person class"'),
        ('# Multi-class files also supported', '"Multi-class files also supported'),
        ('# Object - Root class (all objects inherit from this)', '"Object - Root class (all objects inherit from this)'),
        ('# String, Number, Boolean - Built-in types', '"String, Number, Boolean - Built-in types'),
        ('# Multiline strings (like Nim)', '"Multiline strings (like Nim)'),
        ('# Array literal (ordered)', '"Array literal (ordered)'),
        ('# Table literal (dictionary)', '"Table literal (dictionary)'),
        ('# Get element from array', '"Get element from array'),
        ('# Get value from table', '"Get value from table'),
        ('# Conditional', '"Conditional'),
        ('# Looping', '"Looping'),
        ('# Collection iteration', '"Collection iteration'),
        ('# Empty class', '"Empty class'),
        ('# Dictionary with property bag', '"Dictionary with property bag'),
        ('# Create then init', '"Create then init"'),
        ('# Declare ivars', '"Declare ivars"'),
        ('# Accessor method', '"Accessor method"'),
        ('# Property bag access', '"Property bag access'),
        ('# Property retrieval', '"Property retrieval"'),
        ('# Define unary', '"Define unary"'),
        ('# Define keyword', '"Define keyword"'),
        ('# Multi-keyword', '"Multi-keyword"'),
        ('# Standard way', '"Standard way"'),
        ('# Call dynamically', '"Call dynamically"'),
        ('# Unary', '"Unary"'),
        ('# Keyword', '"Keyword"'),
        ('# Binary', '"Binary"'),
        ('# Inherit + add', '"Inherit + add"'),
        ('# Call parent', '"Call parent"'),
        ('# This is a comment (line, to end)', '"This is a comment (double quotes)'),
        ('#==== Section header', '"==== Section header'),
        ('\"Inline comment\" someCode', "'Inline comment' someCode"),
        ('# Note: Character literals not yet implemented', '"Note: Use symbols for characters'),
    ]

    for old, new in replacements:
        content = content.replace(old, new)

    # Update comparison table
    content = content.replace(
        '| Object Model | Class-based | Class-based |',
        '| Object Model | Class-based | Class-based |'
    )
    content = content.replace(
        '| Inheritance | Classes | Class hierarchy |',
        '| Inheritance | Classes | Class hierarchy |'
    )
    content = content.replace(
        '| Method Storage | Class dictionary | Stored on class |',
        '| Method Storage | Class dictionary | Stored on class |'
    )
    content = content.replace(
        "| String Literals | Single quotes | Single quotes |",
        "| String Literals | Single quotes | Single quotes |"
    )

    return content

def update_smalltalkers(content):
    """Update NIMTALK_FOR_SMALLTALKERS.md"""
    # Update header
    content = content.replace(
        'Nimtalk is a prototype-based Smalltalk dialect',
        'Nimtalk is a class-based Smalltalk dialect'
    )

    # Update quick summary table
    content = content.replace(
        "| String quotes | Single quote (`'`) | Double quote (`\") only |",
        "| String quotes | Single quote (`'`) | Single quote (`'`) |"
    )

    # Update string literals section
    content = content.replace(
        '**Nimtalk:**\n```nimtalk\n"Hello World"       "Double quotes only"\n```',
        "**Nimtalk:**\n```nimtalk\n'Hello World'       'Single quotes'\n```"
    )
    content = content.replace(
        "**Note**: Single quotes are reserved for future use (character literals).",
        "**Note**: Double quotes are used for comments."
    )

    # Update comments section
    content = content.replace(
        '**Nimtalk:**\n```nimtalk\n# This is a comment - hash style only\n#==== Section header\n```',
        '**Nimtalk:**\n```nimtalk\n"This is a comment - double quotes for comments"\n"==== Section header\n```'
    )
    content = content.replace(
        '**Note**: Smalltalk-style `"comment"` syntax is not supported. Double-quoted strings are actual string literals, not comments.',
        '**Note**: Hash-style `# comment` is also supported for section headers.'
    )

    # Update derive examples
    content = content.replace(
        "Person := Object derive: #(name age).",
        "Person := Object derive: #(#name #age)."
    )

    # Update string examples in code
    content = content.replace(
        '"Hello, " , name',
        "'Hello, ' , name"
    )
    content = content.replace(
        '^ "Hello" ]',
        "^ 'Hello' ]"
    )

    return content

def main():
    files = {
        '/home/gokr/tankfeud/nimtalk/docs/SPECIFICATION.md': update_specification,
        '/home/gokr/tankfeud/nimtalk/docs/SYNTAX-REALITY.md': update_syntax_reality,
        '/home/gokr/tankfeud/nimtalk/docs/SYNTAX-QUICKREF-updated.md': update_syntax_quickref,
        '/home/gokr/tankfeud/nimtalk/docs/NIMTALK_FOR_SMALLTALKERS.md': update_smalltalkers,
    }

    for filepath, update_func in files.items():
        try:
            with open(filepath, 'r') as f:
                content = f.read()

            new_content = update_func(content)

            with open(filepath, 'w') as f:
                f.write(new_content)

            print(f"Updated: {filepath}")
        except Exception as e:
            print(f"Error updating {filepath}: {e}")

if __name__ == '__main__':
    main()
