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


proc getTrash*(trashDirDefault: static[string] = ""): string =
  ## Return the operating system Trash directory.
  ## Android has no Trash, some apps just use a temporary folder.
  ## Windows Trash is not supported.
  ## Use `trashDirDefault` for not supported operating systems.
  when trashDirDefault.len > 0:
    result = absolutePath(trashDirDefault)
  elif defined(linux) or defined(bsd):
    result = absolutePath(getEnv("XDG_DATA_HOME", getHomeDir()) / ".local/share/Trash")
  elif defined(osx):
    result = absolutePath(getHomeDir() / ".Trash")
  elif defined(android):
    result = getTempDir()
  else:
    raise newException(ValueError, "Operating system Trash is currently not supported, use trashDirDefault argument")
  if not dirExists(result):
    raise newException(ValueError, "Trash path must exist: " & result)
  if not isAbsolute(result):
    raise newException(ValueError, "Trash path must be absolute: " & result)
  if symlinkExists(result):
    raise newException(ValueError, "Trash path must not be a symlink: " & result)


proc moveFileToTrash*(path: string, trashPath = getTrash();
    postfixRange = 1.Positive..int.high.Positive): string =
  ## Move file from `path` to `trashPath`.
  ##
  ## If a file with the same name already exists in the Trash folder,
  ## then appends a postfix like `" (1)"`, `" (2)"`, `" (3)"`, etc,
  ## returns the path of the file in the Trash, with the generated postfix if any.
  ##
  ## If the latest lowest and/or highest postfix used on the Trash folder is known,
  ## then can be used as `postfixRange`.
  ##
  ## If `postfixStart` and `postfixStop` are provided,
  ## then the file scan loop can be reduced to a single iteration.
  if path.len == 0:
    raise newException(ValueError, "path must not be empty string: " & path)
  if trashPath.len == 0:
    raise newException(ValueError, "trashPath must not be empty string: " & trashPath)

  discard existsOrCreateDir(trashPath)
  let fullPath = absolutePath(path)
  var fname = extractFilename(fullPath)
  result = trashHelper(trashPath, fname)

  # If file exists on Trash, append " (1)", " (2)", " (3)", etc.
  if fileExists(result):
    for i in postfixRange:
      var prefix = " ("
      prefix.add $i
      prefix.add ')'
      if not fileExists(result & prefix):
        fname = fname & prefix
        result = trashHelper(trashPath, fname)
        break
  # Do NOT add an else here, because it only changes filename if it exists.
  when defined(linux) or defined(bsd):
    discard existsOrCreateDir(trashPath / "files")
    discard existsOrCreateDir(trashPath / "info")
    moveFile(fullPath, result)
    writeFile(trashPath / "info" / fname & ".trashinfo",
      trashinfo % [fullPath, now().format("yyyy-MM-dd'T'HH:MM:ss")])
  else:
    moveFile(path, result)


proc moveFileFromTrash*(path: string, trashPath = getTrash()) =
  ## Move file from `trashPath` to `path`.
  runnableExamples:
    import os
    if off:
      let trashedFile = moveFileToTrash("example.txt", "/path/to/trash/folder")
      moveFileFromTrash(getCurrentDir() / extractFilename(trashedFile), "/path/to/trash/folder")

  if path.len == 0:
    raise newException(ValueError, "path must not be empty string: " & path)
  if trashPath.len == 0:
    raise newException(ValueError, "trashPath must not be empty string: " & trashPath)
  let fname = extractFilename(path)
  moveFile(trashHelper(trashPath, fname), path)
  when defined(linux) or defined(bsd):
    removeFile(trashPath / "info" / fname & ".trashinfo")
