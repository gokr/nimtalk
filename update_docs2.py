#!/usr/bin/env python3
"""Update remaining documentation files"""

def update_tools_debugging(content):
    """Update TOOLS_AND_DEBUGGING.md"""
    # Update REPL examples
    content = content.replace(
        'nt> obj at: \'test\' put: 42',
        'nt> obj at: #test put: 42'
    )
    content = content.replace(
        "nt> obj at: 'test'",
        'nt> obj at: #test'
    )
    content = content.replace(
        "test at: 'value' put: 3 + 4.",
        'test at: #value put: 3 + 4.'
    )
    content = content.replace(
        "test at: 'value'  # Should be 7",
        'test at: #value  "Should be 7"'
    )

    return content

def update_gintro(content):
    """Update GINTRO.md"""
    # Update string literals
    content = content.replace(
        "title := 'Untitled Window'.",
        "title := 'Untitled Window'."
    )
    content = content.replace(
        "self title: 'Nimtalk IDE'.",
        "self title: 'Nimtalk IDE'."
    )
    content = content.replace(
        "'New Workspace'",
        "'New Workspace'"
    )
    content = content.replace(
        "'Browser'",
        "'Browser'"
    )
    # These are already single quoted, but update comments if any

    return content

def update_smalltalk80(content):
    """Update SMALLTALK80.md"""
    # Update prototype references
    content = content.replace(
        'Nimtalk is a prototype-based Smalltalk dialect',
        'Nimtalk is a class-based Smalltalk dialect'
    )
    content = content.replace(
        '### Prototype vs Class Model',
        '### Class Model'
    )
    content = content.replace(
        'Smalltalk-80 is class-based with metaclasses. Nimtalk is prototype-based.',
        'Smalltalk-80 is class-based with metaclasses. Nimtalk is class-based.'
    )
    content = content.replace(
        "`species` in Smalltalk returns a class; in Nimtalk it could return a prototype object",
        "`species` in Smalltalk returns a class; in Nimtalk it returns a class object"
    )
    content = content.replace(
        'Class methods become constructor methods on prototypes',
        'Class methods become constructor methods on classes'
    )

    # Update string examples
    content = content.replace(
        "#('a' 'b' 'c')",
        "#('a' 'b' 'c')"  # Already correct
    )

    return content

def update_newline_rules(content):
    """Update NEWLINE_RULES.md"""
    content = content.replace(
        "ifTrue: [ ^ 'Object' ]",
        "ifTrue: [ ^ 'Object' ]"  # Already single quoted
    )
    return content

def update_greenthreads(content):
    """Update GREENTHREADS.md"""
    content = content.replace(
        'common `rootObject` (the prototype hierarchy root)',
        'common `rootObject` (the class hierarchy root)'
    )
    content = content.replace(
        '# Share prototype root',
        '"Share class root'
    )
    return content

def main():
    files = {
        '/home/gokr/tankfeud/nimtalk/docs/TOOLS_AND_DEBUGGING.md': update_tools_debugging,
        '/home/gokr/tankfeud/nimtalk/docs/GINTRO.md': update_gintro,
        '/home/gokr/tankfeud/nimtalk/docs/SMALLTALK80.md': update_smalltalk80,
        '/home/gokr/tankfeud/nimtalk/docs/NEWLINE_RULES.md': update_newline_rules,
        '/home/gokr/tankfeud/nimtalk/docs/GREENTHREADS.md': update_greenthreads,
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
