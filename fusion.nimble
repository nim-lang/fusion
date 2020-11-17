# Package

version       = "1.0"
author        = "Araq"
description   = "Extensions for Nim's stdlib"
license       = "MIT"
srcDir        = "src"


# Dependencies
requires "nim >= 1.0.0"

task docs, "":
  # can customize, e.g.:
  # exec "nim r src/fusion/docutils " & srcDir & " --outdir:htmldocs2 -d:foo"
  exec "nim c -r src/fusion/docutils " & srcDir
