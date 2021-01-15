#[
## Design rationale
* an intermediate `GlobOpt` is used to allow easier proc forwarding
* a yieldFilter, regex match etc isn't needed because caller can filter at
  call site, without loss of generality, unlike `follow`; this simplifies the API.

## Future work:
* provide a way to do error reporting, which is tricky because iteration cannot be resumed
]#

import std/[os, algorithm, deques, macros]

type
  PathEntrySub* = object
    kind*: PathComponent
    path*: string
  PathEntry* = object
    kind*: PathComponent
    path*: string
      ## absolute or relative path wrt globbed dir
    depth*: int
      ## depth wrt GlobOpt.dir (which is at depth 0)
    epilogue*: bool
  GlobMode* = enum
    gDfs ## depth first search
    gBfs ## breadth first search
  FollowCallback* = proc(entry: PathEntry): bool
  SortCmpCallback* = proc (x, y: PathEntrySub): int
  GlobOpt* = object
    dir*: string ## root of glob
    relative: bool ## when true, paths are are returned relative to `dir`, else they start with `dir`
    checkDir: bool ## if true, raises `OSError` when `dir` can't be listed. Deeper
      ## directories do not cause `OSError`, and currently no error reporting is done for those.
    globMode: GlobMode ## controls how paths are returned
    includeRoot: bool ## whether to include root `dir`
    includeEpilogue: bool
      ## when false, yields: someDir, <children of someDir>
      ## when true, yields: someDir, <children of someDir>, someDir: each dir is
      ## yielded a 2nd time. This is useful in applications that aggregate data over dirs.
    followSymlinks: bool ## whether to follow symlinks
    follow: FollowCallback
      ## if not `nil`, `glob` visits `entry` if `follow(entry) == true`.
    sortCmp: SortCmpCallback
      ## if not `nil`, immediate children of a dir are sorted using `sortCmp`

macro ctor(obj: untyped, a: varargs[untyped]): untyped =
  ##[
  generates an object constructor call from a list of fields
  ]##
  # xxx expose in some `fusion/macros`
  runnableExamples:
    type Foo = object
      a, b: int
    doAssert Foo.ctor(a,b) == Foo(a: a, b: b)
  result = nnkObjConstr.newTree(obj)
  for ai in a: result.add nnkExprColonExpr.newTree(ai, ai)

proc initGlobOpt*(
  dir: string, relative = false, checkDir = true, globMode = gDfs,
  includeRoot = false, includeEpilogue = false, followSymlinks = false,
  follow: FollowCallback = nil, sortCmp: SortCmpCallback = nil): GlobOpt =
  GlobOpt.ctor(dir, relative, checkDir, globMode, includeRoot, includeEpilogue, followSymlinks, follow, sortCmp)
# import timn/dbgs # PRTEMP
iterator globOpt*(opt: GlobOpt): PathEntry =
  ##[
  Recursively walks `dir`.
  This is more flexible than `os.walkDirRec`.
  ]##
  runnableExamples:
    import os,sugar
    if false: # see also `tfilewalks.nim`
      # list hidden files of depth <= 2 + 1 in your home.
      for e in glob(getHomeDir(), follow = a=>a.path.isHidden and a.depth <= 2):
        if e.kind in {pcFile, pcLinkToFile}: echo e.path

  var entry = PathEntry(depth: 0, path: ".")
  when nimvm: entry.kind = pcDir
  else: # xxx support `symlinkExists` in nimvm
    entry.kind = if symlinkExists(opt.dir): pcLinkToDir else: pcDir
  var stack = initDeque[PathEntry]()

  var checkDir = opt.checkDir
  if dirExists(opt.dir):
    stack.addLast entry
  elif checkDir:
    # dbg dirExists(opt.dir)
    raise newException(OSError, "invalid root dir: " & opt.dir)

  var dirsLevel: seq[PathEntrySub]
  while stack.len > 0:
    let current = if opt.globMode == gDfs: stack.popLast() else: stack.popFirst()
    entry.epilogue = current.epilogue
    entry.depth = current.depth
    entry.kind = current.kind
    entry.path = if opt.relative: current.path else: opt.dir / current.path
    normalizePath(entry.path) # pending https://github.com/timotheecour/Nim/issues/343

    if opt.includeRoot or current.depth > 0:
      yield entry
    # let isSort = opt.sortCmp != nil # hits: bug #15595
    let isSort = not opt.sortCmp.isNil

    if (current.kind == pcDir or current.kind == pcLinkToDir and opt.followSymlinks) and not current.epilogue:
      # if opt.follow == nil or opt.follow(current):
      if opt.follow.isNil or opt.follow(current):
        if isSort:
          dirsLevel.setLen 0
        if opt.includeEpilogue:
          stack.addLast PathEntry(depth: current.depth, path: current.path, kind: current.kind, epilogue: true)
        # checkDir is still needed here in first iteration because things could
        # fail for reasons other than `not dirExists`.
        # for k, p in walkDir(opt.dir / current.path, relative = true, checkDir = checkDir):
        for k, p in walkDir(opt.dir / current.path, relative = true):
          if isSort:
            dirsLevel.add PathEntrySub(kind: k, path: p)
          else:
            stack.addLast PathEntry(depth: current.depth + 1, path: current.path / p, kind: k)
        checkDir = false
          # We only check top-level dir, otherwise if a subdir is invalid (eg. wrong
          # permissions), it'll abort iteration and there would be no way to resume iteration.
        if isSort:
          sort(dirsLevel, opt.sortCmp)
          for i in 0..<dirsLevel.len:
            let j = if opt.globMode == gDfs: dirsLevel.len-1-i else: i
            let ai = dirsLevel[j]
            stack.addLast PathEntry(depth: current.depth + 1, path: current.path / ai.path, kind: ai.kind)

template glob*(args: varargs[untyped]): untyped =
  ## convenience wrapper
  globOpt(initGlobOpt(args))
  # let opt = initGlobOpt(args)
  # for ai in 
