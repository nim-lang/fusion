import std/os


template withDir*(dir: string, body: untyped): untyped =
  ## Changes the current directory temporarily.
  ## Usage example:
  ##
  ## .. code-block:: nim
  ##   withDir "foo":
  ##     # inside foo directory
  ##   # back to last directory
  let curDir = getCurrentDir()
  setCurrentDir(dir)
  body
  setCurrentDir(curDir)
