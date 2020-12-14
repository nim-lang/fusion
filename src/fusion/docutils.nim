import std/[os,strformat,sugar,osproc]
import std/private/globs

iterator findNimSrcFiles*(dir: string): string =
  proc follow(a: PathEntry): bool =
    a.path.lastPathPart notin ["nimcache", "htmldocs", "js"]
  for entry in walkDirRecFilter(dir, follow = follow):
    if entry.path.splitFile.ext == ".nim" and entry.kind == pcFile:
      yield entry.path

proc genCodeImportAll*(dir: string): string =
  result = "{.warning[UnusedImport]: off.}\n"
  for a in findNimSrcFiles(dir):
    let s = "".dup(addQuoted(a))
    result.add &"import {s}\n"

proc genDocs(dir: string, nim = "", args: seq[string]) =
  let code = genCodeImportAll(dir)
  let extra = quoteShellCommand(args)
  let nim = if nim.len == 0: getCurrentCompilerExe() else: nim
  let ret = execCmdEx(fmt"{nim} doc -r --project --docroot --outdir:htmldocs {extra} -", input = code)
  if ret.exitCode != 0:
    doAssert false, ret.output & "\n" & code

when isMainModule:
  let args = commandLineParams()
  doAssert args.len >= 1
  let dir = args[0]
  genDocs(dir, nim="", args[1..^1])
