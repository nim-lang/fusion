## Convenience procedures for the operating system Trash support.
## Allows to trash several files with the same name.
##
## * http://www.freedesktop.org/wiki/Specifications/trash-spec
## * http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
when defined(js):
  {.fatal: "Module trash is not designed to be used with the JavaScript backend.".}

import os, times, strutils


const trashinfo = """
[Trash Info]
Path=$1
DeletionDate=$2
"""


template trashHelper(trashPath: string; fname: string): string =
  when defined(linux) or defined(bsd): trashPath / "files" / fname
  else: trashPath / fname


proc moveFileToTrash*(path, trashPath: string;
    postfixStart = 1.Positive; postfixStop = int.high.Positive): string =
  ## Move file from `path` to `trashPath`.
  ##
  ## If a file with the same name already exists in the Trash folder,
  ## then appends a postfix like `" (1)"`, `" (2)"`, `" (3)"`, etc,
  ## returns the path of the file in the Trash, with the generated postfix if any.
  ##
  ## If the latest lowest postfix used on the Trash folder is known,
  ## then can be used as `postfixStart`.
  ##
  ## If the latest highest postfix used on the Trash folder is known,
  ## then can be used as `postfixStop`.
  ##
  ## If `postfixStart` and `postfixStop` are provided,
  ## then the file scan loop can be reduced to a single iteration.
  assert path.len > 0, "path must not be empty string"
  assert trashPath.len > 0, "trashPath must not be empty string"
  assert postfixStop > postfixStart
  discard existsOrCreateDir(trashPath)
  let fullPath = expandFilename(path)
  var fname = extractFilename(fullPath)
  result = trashHelper(trashPath, fname)

  # If file exists on Trash, append " (1)", " (2)", " (3)", etc.
  if fileExists(result):
    for i in postfixStart .. postfixStop:
      var prefix = " ("
      prefix.add $i
      prefix.add ')'
      if not fileExists(result & prefix):
        fname = fname & prefix
        result = trashHelper(trashPath, fname)
        break
  else:
    raise newException(IOError, "File not found: " & result)

  when defined(linux) or defined(bsd):
    discard existsOrCreateDir(trashPath / "files")
    discard existsOrCreateDir(trashPath / "info")
    moveFile(fullPath, result)
    writeFile(trashPath / "info" / fname & ".trashinfo",
      trashinfo % [fullPath, now().format("yyyy-MM-dd'T'HH:MM:ss")])
  else:
    moveFile(path, result)


proc moveFileFromTrash*(path: string; trashPath: string) =
  ## Move file from `trashPath` to `path`.
  runnableExamples:
    import os
    if off:
      let trashedFile = moveFileToTrash("example.txt", "/path/to/trash/folder")
      moveFileFromTrash(getCurrentDir() / extractFilename(trashedFile), "/path/to/trash/folder")

  assert path.len > 0, "path must not be empty string"
  assert trashPath.len > 0, "trashPath must not be empty string"
  if dirExists(trashPath):
    when defined(linux) or defined(bsd):
      let fname = extractFilename(path)
      moveFile(trashHelper(trashPath, fname), path)
      removeFile(trashPath / "info" / fname & ".trashinfo")
    else:
      moveFile(trashPath / extractFilename(path), path)
  else:
    raise newException(IOError, "Directory not found: " & trashPath)