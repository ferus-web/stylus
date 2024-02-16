# Package

version       = "0.1.0"
author        = "xTrayambak"
description   = "A CSS level 3 parser"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 2.0.0"
requires "results >= 0.4.0"
requires "nph >= 0.3.0"

before build:
  exec "nph src/"
