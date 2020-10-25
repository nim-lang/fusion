import std/[sugar,os,strutils,sequtils,algorithm]
from std/private/globs import nativeToUnixPath

from tfusion/paths import buildDir

import fusion/filewalks

proc processAux[T](a: T): seq[string] =
  a.mapIt(it.path.nativeToUnixPath)

proc process[T](a: T): seq[string] =
  a.processAux.sorted

const dir = buildDir/"tfilewalks"

proc test() =
  block: # follow
    # filter by pcFile
    doAssert toSeq(glob(dir, follow = a=>a.path.lastPathPart != "d1b", relative = true))
      .filterIt(it.kind == pcFile).process == @["d1/d1a/f2.txt", "d1/d1a/f3", "d1/f1.txt", "f5"]
    # filter by pcDir
    doAssert toSeq(glob(dir, relative = true))
      .filterIt(it.kind == pcDir).process == @["d1", "d1/d1a", "d1/d1a/d1a1", "d1/d1b", "d1/d1b/d1b1", "d2"]

  block: # includeRoot
    doAssert toSeq(glob(dir, relative = true, includeRoot = true))
    .filterIt(it.kind == pcDir).process == @[".", "d1", "d1/d1a", "d1/d1a/d1a1", "d1/d1b", "d1/d1b/d1b1", "d2"]

  block: # checkDir
    doAssertRaises(OSError): discard toSeq(glob("nonexistant"))
    doAssertRaises(OSError): discard toSeq(glob("f5"))
    doAssert toSeq(glob("nonexistant", checkDir = false)) == @[]

  # sortCmp
  proc mySort(a, b: PathEntrySub): int = cmp(a.path, b.path)
  doAssert toSeq(glob(dir, relative = true, sortCmp = mySort)).processAux ==
    @["d1", "d1/d1a", "d1/d1a/d1a1", "d1/d1a/f2.txt", "d1/d1a/f3", "d1/d1b", "d1/d1b/d1b1", "d1/d1b/d1b1/f4", "d1/f1.txt", "d2", "f5"]

  # gBfs
  doAssert toSeq(glob(dir, relative = true, sortCmp = mySort, globMode = gBfs)).processAux ==
    @["d1", "d2", "f5", "d1/d1a", "d1/d1b", "d1/f1.txt", "d1/d1a/d1a1", "d1/d1a/f2.txt", "d1/d1a/f3", "d1/d1b/d1b1", "d1/d1b/d1b1/f4"]

  # includeEpilogue
  doAssert toSeq(glob(dir, relative = true, sortCmp = mySort, includeEpilogue = true, includeRoot = true)).processAux ==
    @[".", "d1", "d1/d1a", "d1/d1a/d1a1", "d1/d1a/d1a1", "d1/d1a/f2.txt", "d1/d1a/f3", "d1/d1a", "d1/d1b", "d1/d1b/d1b1", "d1/d1b/d1b1/f4", "d1/d1b/d1b1", "d1/d1b", "d1/f1.txt", "d1", "d2", "d2", "f5", "."]

when defined(fusionTfilewalksTesting):
  when (NimMajor, NimMinor, NimPatch) >= (1, 5, 3):
    # pending https://github.com/nim-lang/Nim/pull/15705
    static: test()
  test()
else:
  from tfusion/osutils import genTestPaths
  import std/strformat
  proc main() =
    defer: removeDir(dir)
    let paths = """
d1/f1.txt
d1/d1a/f2.txt
d1/d1a/f3
d1/d1a/d1a1/
d1/d1b/d1b1/f4
d2/
f5
""".splitLines.filter(a=>a.len>0)
    genTestPaths(dir, paths)
    const nim = getCurrentCompilerExe()
    const input = currentSourcePath()
    let cmd = &"{nim} c -r -d:fusionTfilewalksTesting {input}"
    let status = execShellCmd(cmd)
    doAssert status == 0
  main()
