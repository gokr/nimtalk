# begin Nimble config (version 2)
when withDir(thisDir(), system.fileExists("nimble.paths")):
  include "nimble.paths"
# end Nimble config

# Use refc instead of ORC to avoid stack overflow during cleanup
# of deeply nested interpreter object graphs.
# See CLAUDE.md for details on ORC issues with cyclic references.
switch("mm", "refc")

# Add src to path for proper module resolution
switch("path", "src")
