## Convenience procedures for the operating system Trash support.
## Allows to trash several files with the same name.
##
## * http://www.freedesktop.org/wiki/Specifications/trash-spec
## * http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html
when defined(js):
  {.fatal: "Module trash is not designed to be used with the JavaScript backend.".}

import os, times


proc getTrash*(): string =
  ## Return the operating system Trash directory.
  ## Android has no Trash, some apps just use a temporary folder.
  ## Windows Trash is not supported.
  result =
    when defined(linux) or defined(bsd):
      getEnv("XDG_DATA_HOME", getHomeDir()) / ".local/share/Trash"
    elif defined(osx):
      getEnv("HOME", getHomeDir()) / ".Trash"
    elif defined(android):
      getTempDir()
    else:
      doAssert false, "Operating system Trash is currently not supported"


proc moveFileToTrash*(path: string; trashPath = getTrash(); postfixStart = 1.Positive): string =
  ## Move file from `path` to `trashPath`, `trashPath` defaults to `getTrash()`.
  ##
  ## If a file with the same name already exists in the Trash folder,
  ## then appends a postfix like `" (1)"`, `" (2)"`, `" (3)"`, etc,
  ## returns the path of the file in the Trash, with the generated postfix if any.
  ##
  ## If you know the latest highest postfix used on the Trash folder,
  ## then you can use `postfixStart` for better performance.
  assert path.len > 0, "path must not be empty string"
  discard existsOrCreateDir(trashPath)
  let fullPath = expandFilename(path)
  var fname = extractFilename(fullPath)
  result =
    when defined(linux) or defined(bsd): trashPath / "files" / fname
    else: trashPath / fname
  # If file exists on Trash, append " (1)", " (2)", " (3)", etc.
  if fileExists(result):
    for i in postfixStart .. int.high:
      var prefix = " ("
      prefix.add $i
      prefix.add ')'
      if not fileExists(result & prefix):
        fname = fname & prefix
        result =
          when defined(linux) or defined(bsd): trashPath / "files" / fname
          else: trashPath / fname
        break
  when defined(linux) or defined(bsd):
    var trashinfo = "[Trash Info]\nPath="
    trashinfo.add fullPath
    trashinfo.add "\nDeletionDate="
    trashinfo.add now().format("yyyy-MM-dd'T'HH:MM:ss")
    trashinfo.add '\n'
    discard existsOrCreateDir(trashPath / "files")
    discard existsOrCreateDir(trashPath / "info")
    moveFile(fullPath, result)
    writeFile(trashPath / "info" / fname & ".trashinfo", trashinfo)
  else:
    moveFile(path, result)


proc moveFileFromTrash*(path: string; trashPath = getTrash()) =
  ## Move file from `trashPath` to `path`, `trashPath` defaults to `getTrash()`.
  ##
  ## If a file with the same name already exists in the Trash folder,
  ## then the generated postfix like `" (1)"`, `" (2)"`, `" (3)"`, etc,
  ## is required on `path` to restore the correct file.
  ##
  ## .. code-block:: nim
  ##   import os, trash
  ##   writeFile("example.txt", "")
  ##   let trashedFile = moveFileToTrash("example.txt")
  ##   moveFileFromTrash(getCurrentDir() / extractFilename(trashedFile))
  assert path.len > 0, "path must not be empty string"
  if dirExists(trashPath):
    when defined(linux) or defined(bsd):
      let fname = extractFilename(path)
      moveFile(trashPath / "files" / fname, path)
      discard tryRemoveFile(trashPath / "info" / fname & ".trashinfo")
    else:
      moveFile(trashPath / extractFilename(path), path)
