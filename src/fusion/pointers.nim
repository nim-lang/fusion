##[
Convenience procs to deal with pointer-like variables.
]##

proc toUncheckedArray*[T](a: ptr T): ptr UncheckedArray[T] {.inline.} =
  ## Shortcut for `cast[ptr UncheckedArray[T]](a)`, where T is inferred.
  ## This allows array indexing operations on `a`.
  ## This is unsafe as it returns `UncheckedArray`.
  runnableExamples:
    var a = @[10, 11, 12]
    let pa = a[1].addr.toUncheckedArray
    doAssert pa[-1] == 10
    pa[0] = 100
    doAssert a == @[10, 100, 12]
    pa[0] += 5
    doAssert a[1] == 105
  cast[ptr UncheckedArray[T]](a)

template `+`*[T](p: ptr T, off: int): ptr T =
  runnableExamples:
    var a = @[10, 11, 12]
    let pa = a[0].addr
    doAssert (pa + 1)[] == 11
    doAssert pa[2] == 12
    pa[1] = 2
    doAssert a[1] == 2
  type T = typeof(p[]) # pending https://github.com/nim-lang/Nim/issues/13527
  cast[ptr T](cast[ByteAddress](p) +% off * sizeof(T))

template `-`*[T](p: ptr T, off: int): ptr T =
  type T = typeof(p[])
  cast[ptr T](cast[ByteAddress](p) -% off * sizeof(T))

template `[]`*[T](p: ptr T, off: int): T =
  (p + off)[]

template `[]=`*[T](p: ptr T, off: int, val: T) =
  (p + off)[] = val

proc `+=`*[T](p: var ptr T, off: int) {.inline.} =
  # not a template to avoid double evaluation issues
  p = p + off

proc `-=`*[T](p: var ptr T, off: int) {.inline.} =
  p = p - off
