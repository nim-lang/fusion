import std/os

const
  fusionRoot* = currentSourcePath.parentDir.parentDir.parentDir.parentDir
  buildDir* = fusionRoot / "build"
  nimbleFile = fusionRoot / "fusion.nimble"

static: doAssert nimbleFile.fileExists # sanity check

when isMainModule:
  echo buildDir
