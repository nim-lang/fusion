# Package

version       = "1.0"
author        = "Araq"
description   = "Extensions for Nim's stdlib"
license       = "MIT"
srcDir        = "src"


# Dependencies
requires "nim >= 1.0.0"

task docs, "":
  # JavaScript
  when (NimMajor, NimMinor) >= (1, 5):
    exec "nim c -r -d:fusionDocJs src/fusion/docutils " & srcDir
  # C
  exec "nim c -r src/fusion/docutils " & srcDir

task testMulti, "Execute test suite with different GC options":
  exec "nimble test"
  when (NimMajor, NimMinor) >= (1, 2):
    exec "nimble test --gc:arc"
    exec "nimble test --gc:orc"
