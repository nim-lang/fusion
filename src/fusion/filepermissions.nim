import os

func toFilePermissions*(perm: Natural): set[FilePermission] =
  ## Convenience func to convert Unix like file permission to ``set[FilePermission]``.
  ##
  ## See also:
  ## * `getFilePermissions <#getFilePermissions,string>`_
  ## * `setFilePermissions <#setFilePermissions,string,set[FilePermission]>`_
  runnableExamples:
    import os
    doAssert toFilePermissions(0o700) == {fpUserExec, fpUserRead, fpUserWrite}
    doAssert toFilePermissions(0o070) == {fpGroupExec, fpGroupRead, fpGroupWrite}
    doAssert toFilePermissions(0o007) == {fpOthersExec, fpOthersRead, fpOthersWrite}
    doAssert toFilePermissions(0o644) == {fpUserWrite, fpUserRead, fpGroupRead, fpOthersRead}
    doAssert toFilePermissions(0o777) == {fpUserExec, fpUserWrite, fpUserRead, fpGroupExec, fpGroupWrite, fpGroupRead, fpOthersExec, fpOthersWrite, fpOthersRead}
    doAssert toFilePermissions(0o000) == {}
  var perm = uint(perm)
  for permBase in [fpOthersExec, fpGroupExec, fpUserExec]:
    if (perm and 1) != 0: result.incl permBase         # Exec
    if (perm and 2) != 0: result.incl permBase.succ()  # Read
    if (perm and 4) != 0: result.incl permBase.succ(2) # Write
    perm = perm shr 3  # Shift to next permission group


proc chmod*(path: string; permissions: Natural) {.inline.} =
  ## Convenience proc for `os.setFilePermissions("file.ext", filepermissions.toFilePermissions(0o666))`
  ## to change file permissions using Unix like octal file permission.
  ##
  ## See also:
  ## * `setFilePermissions <#setFilePermissions,string,set[FilePermission]>`_
  setFilePermissions(path, toFilePermissions(permissions))
