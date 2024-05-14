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

template `cast`*[T](a:T, T2: typedesc): untyped =
  ## Same as `cast[T2](a)` but can be used in UFCS/MCS chains.
  ## Unsafe as it calls `cast`.
  runnableExamples:
    var a = [1'u8, 2, 3, 4]
    # the MCS chain reads left to right:
    let x1 = a.cast(array[2, uint16])[0].cast(char)
    # instead of a zig-zag:
    let x2 = cast[char](cast[array[2, uint16]](a)[0])
    doAssert x1 == x2
  cast[T2](a)

template addrCast*[T](a:T, T2: typedesc): untyped =
  ## Same as `cast[ptr T2](unsafeAddr(a))[]` but can be used in UFCS/MCS chains.
  ## Unsafe as it calls `cast`.
  runnableExamples:
    proc fn(a: var auto): auto =
      a[0].inc
      a
    var a = [1'u8, 2, 3, 4]
    let b = fn(a.addrCast(array[2, uint16]))
    doAssert a == [2'u8, 2, 3, 4]
    when system.cpuEndian == littleEndian:
      doAssert b == [514'u16, 1027]
      doAssert a.addrCast(uint32) == 67305986'u32
      a.addrCast(uint32).inc
      doAssert a == [3'u8, 2, 3, 4]
  cast[ptr T2](unsafeAddr(a))[]
