import ../src/nimtalk/parser/lexer
import ../src/nimtalk/parser/parser
import ../src/nimtalk/core/types

let source = """
TestObject := Object derive

TestObject at: #add:and: put: [
  <primitive>
  # Nim implementation of addition
  let a = args[0].intVal
  let b = args[1].intVal
  return NodeValue(kind: vkInt, intVal: a + b)
  </primitive>
  # Smalltalk fallback
  ^ self error: 'Primitive failed'
].
"""

echo "Lexing..."
let tokens = lex(source)
echo "Tokens: ", tokens.len
for i, t in tokens:
  echo i, ": ", t

echo "\nParsing..."
let (nodes, parseResult) = parse(source)
if parseResult.hasError:
  echo "Parse error: ", parseResult.errorMsg
else:
  echo "Parsed ", nodes.len, " nodes"
  for i, node in nodes:
    echo "Node ", i, ":"
    echo printAST(node)