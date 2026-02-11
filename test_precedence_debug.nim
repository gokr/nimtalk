import std/strutils
import src/harding/parser/lexer
import src/harding/parser/parser
import src/harding/core/types

proc main() =
  # Test case 1: Simple binary in keyword arg
  let code1 = "arr at: 1 + 2"
  echo "=== Test 1: ", code1
  let tokens1 = lex(code1)
  echo "Tokens:"
  for i, t in tokens1:
    echo "  ", i, ": ", t.kind, " = '", t.value, "'"

  var parser1 = initParser(tokens1)
  let nodes1 = parser1.parseStatements()
  echo "Parsed ", nodes1.len, " node(s)"
  if nodes1.len > 0:
    echo "AST: ", nodes1[0].kind
    if nodes1[0].kind == nkMessage:
      let msg = MessageNode(nodes1[0])
      echo "  selector: ", msg.selector
      echo "  receiver: ", msg.receiver.kind
      echo "  arguments: ", msg.arguments.len
      for i, arg in msg.arguments:
        echo "    arg[", i, "]: ", arg.kind
        if arg.kind == nkMessage:
          let argMsg = MessageNode(arg)
          echo "      selector: ", argMsg.selector
          echo "      left: ", argMsg.receiver.kind
          if argMsg.arguments.len > 0:
            echo "      right: ", argMsg.arguments[0].kind

  echo ""

  # Test case 2: Comma in keyword arg
  let code2 = "out writeline: \"A\", \"B\""
  echo "=== Test 2: ", code2
  let tokens2 = lex(code2)
  echo "Tokens:"
  for i, t in tokens2:
    echo "  ", i, ": ", t.kind, " = '", t.value, "'"

  var parser2 = initParser(tokens2)
  let nodes2 = parser2.parseStatements()
  echo "Parsed ", nodes2.len, " node(s)"
  if nodes2.len > 0:
    echo "AST: ", nodes2[0].kind
    if nodes2[0].kind == nkMessage:
      let msg = MessageNode(nodes2[0])
      echo "  selector: ", msg.selector
      echo "  receiver: ", msg.receiver.kind
      echo "  arguments: ", msg.arguments.len
      for i, arg in msg.arguments:
        echo "    arg[", i, "]: ", arg.kind

  if parser2.hasError:
    echo "Parse error: ", parser2.errorMsg

  echo ""

  # Test case 3: Working case from test
  let code3 = "calc calculate: 10 + 5 * 2 - 3"
  echo "=== Test 3: ", code3
  let tokens3 = lex(code3)
  echo "Tokens:"
  for i, t in tokens3:
    echo "  ", i, ": ", t.kind, " = '", t.value, "'"

  var parser3 = initParser(tokens3)
  let nodes3 = parser3.parseStatements()
  echo "Parsed ", nodes3.len, " node(s)"
  if nodes3.len > 0:
    echo "AST: ", nodes3[0].kind
    if nodes3[0].kind == nkMessage:
      let msg = MessageNode(nodes3[0])
      echo "  selector: ", msg.selector
      echo "  receiver: ", msg.receiver.kind
      echo "  arguments: ", msg.arguments.len
      for i, arg in msg.arguments:
        echo "    arg[", i, "]: ", arg.kind

  if parser3.hasError:
    echo "Parse error: ", parser3.errorMsg

when isMainModule:
  main()
