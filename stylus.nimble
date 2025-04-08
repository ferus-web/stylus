# Package

version       = "0.1.3"
author        = "xTrayambak"
description   = "A CSS level 3 parser"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 2.0.0"
requires "results >= 0.5.0"
taskRequires "fmt", "nph >= 0.3.0"

task fmt, "Format the codebase":
  exec "nph src/"
