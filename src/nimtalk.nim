## Nimtalk Entry Point
## This is the main module that exports all Nimtalk functionality

# Export core modules
import nimtalk/core/[types]
import nimtalk/parser/[lexer, parser]
import nimtalk/interpreter/[evaluator, objects, activation]
import nimtalk/repl/[ntalk, doit, interact]
import nimtalk/compiler/[codegen]

export types, lexer, parser, evaluator, objects, activation, ntalk, doit, interact, codegen

# Convenience proc to create and run interpreter
proc run*(code: string): string =
  ## Run Nimtalk code and return result as string
  var interp = newInterpreter()
  initGlobals(interp)

  let (result, err) = interp.doit(code)
  if err.len > 0:
    return "Error: " & err
  else:
    return result.toString()

# Version constant
const VERSION* = block:
  const nimblePath = currentSourcePath().parentDir() / "nimtalk.nimble"
  const nimbleContent = staticRead(nimblePath)
  var versionStr = "unknown"
  for line in nimbleContent.splitLines():
    let trimmed = line.strip()
    if trimmed.startsWith("version"):
      let parts = trimmed.split("=")
      if parts.len >= 2:
        versionStr = parts[1].strip().strip(chars={'\"', '\''})
        break
  versionStr

# Simple evaluation demo
when isMainModule:
  echo "Nimtalk v" & VERSION
  echo "Use 'nimble build' to build the REPL"
