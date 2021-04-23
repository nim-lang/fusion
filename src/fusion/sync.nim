import std / locks

when not compileOption("threads"):
  {.error: "This module requires --threads:on compilation flag".}

{.push stackTrace: off.}

type
  Arc*[T] = object
    val: ptr tuple[value: T, atomicCounter: int]

proc `=destroy`*[T](p: var Arc[T]) =
  mixin `=destroy`
  if p.val != nil:
    if atomicLoadN(addr p.val[].atomicCounter, AtomicConsume) == 0:
      `=destroy`(p.val[])
      deallocShared(p.val)
    else:
      discard atomicDec(p.val[].atomicCounter)

proc `=copy`*[T](dest: var Arc[T], src: Arc[T]) =
  if src.val != nil:
    discard atomicInc(src.val[].atomicCounter)
  if dest.val != nil:
    `=destroy`(dest)
  dest.val = src.val

proc newArc*[T](val: sink T): Arc[T] {.nodestroy.} =
  result.val = cast[typeof(result.val)](allocShared(sizeof(result.val[])))
  result.val.atomicCounter = 0
  result.val.value = val

proc isNil*[T](p: Arc[T]): bool {.inline.} =
  p.val == nil

proc `[]`*[T](p: Arc[T]): var T {.inline.} =
  when compileOption("boundChecks"):
    doAssert(p.val != nil, "deferencing nil shared pointer")
  p.val.value

proc `$`*[T](p: Arc[T]): string {.inline.} =
  if p.val == nil: "Arc[" & $T & "](nil)"
  else: "Arc[" & $T & "](" & $p.val.value & ")"

type
  SpinLock* = object
    lock: bool

proc acquire*(s: var SpinLock) =
  while true:
    if not atomicExchangeN(addr s.lock, true, AtomicAcquire):
      return
    else:
      while atomicLoadN(addr s.lock, AtomicRelaxed): cpuRelax()

proc tryAcquire*(s: var SpinLock): bool =
  result = not atomicLoadN(addr s.lock, AtomicRelaxed) and
      not atomicExchangeN(addr s.lock, true, AtomicAcquire)

proc release*(s: var SpinLock) =
  atomicStoreN(addr s.lock, false, AtomicRelease)

template withLock*(a: SpinLock, body: untyped) =
  acquire(a)
  try:
    body
  finally:
    release(a)

type
  Once* = object
    L: Lock
    finished: bool

proc initOnce*(o: var Once) =
  initLock(o.L)
  o.finished = false

proc destroyOnce*(o: var Once) {.inline.} =
  deinitLock(o.L)

template once*(o: Once, body: untyped) =
  if not atomicLoadN(addr o.finished, AtomicAcquire):
    acquire o.L
    try:
      if not o.finished:
        try:
          body
        finally:
          atomicStoreN(addr o.finished, true, AtomicRelease)
    finally:
      release o.L

type
  Semaphore* = object
    c: Cond
    L: Lock
    counter: int

proc initSemaphore*(s: var Semaphore; value: Natural = 0) =
  initCond(s.c)
  initLock(s.L)
  s.counter = value

proc destroySemaphore*(s: var Semaphore) {.inline.} =
  deinitCond(s.c)
  deinitLock(s.L)

proc blockUntil*(s: var Semaphore) =
  acquire(s.L)
  while s.counter <= 0:
    wait(s.c, s.L)
  dec s.counter
  release(s.L)

proc signal*(s: var Semaphore) =
  acquire(s.L)
  inc s.counter
  signal(s.c)
  release(s.L)

type
  Barrier* = object
    c: Cond
    L: Lock
    required: int # number of threads needed for the barrier to continue
    left: int # current barrier count, number of threads still needed.
    cycle: uint # generation count

proc initBarrier*(b: var Barrier; count: Natural) =
  b.required = count
  b.left = count
  b.cycle = 0
  initCond(b.c)
  initLock(b.L)

proc destroyBarrier*(b: var Barrier) {.inline.} =
  deinitCond(b.c)
  deinitLock(b.L)

proc wait*(b: var Barrier) =
  acquire(b.L)
  dec b.left
  if b.left == 0:
    inc b.cycle
    b.left = b.required
    broadcast(b.c)
  else:
    let cycle = b.cycle
    while cycle == b.cycle:
      wait(b.c, b.L)
  release(b.L)

type
  RwMonitor* = object
    readPhase: Cond
    writePhase: Cond
    L: Lock
    counter: int # can be in three states: free = 0, reading > 0, writing = -1

proc initRwMonitor*(rw: var RwMonitor) =
  initCond rw.readPhase
  initCond rw.writePhase
  initLock rw.L
  rw.counter = 0

proc destroyRwMonitor*(rw: var RwMonitor) {.inline.} =
  deinitCond(rw.readPhase)
  deinitCond(rw.writePhase)
  deinitLock(rw.L)

proc beginRead*(rw: var RwMonitor) =
  acquire(rw.L)
  while rw.counter == -1:
    wait(rw.readPhase, rw.L)
  inc rw.counter
  release(rw.L)

proc beginWrite*(rw: var RwMonitor) =
  acquire(rw.L)
  while rw.counter != 0:
    wait(rw.writePhase, rw.L)
  rw.counter = -1
  release(rw.L)

proc endRead*(rw: var RwMonitor) =
  acquire(rw.L)
  dec rw.counter
  if rw.counter == 0:
    rw.writePhase.signal()
  release(rw.L)

proc endWrite*(rw: var RwMonitor) =
  acquire(rw.L)
  rw.counter = 0
  rw.readPhase.broadcast()
  rw.writePhase.signal()
  release(rw.L)

template readWith*(a: RwMonitor, body: untyped) =
  beginRead(a)
  try:
    body
  finally:
    endRead(a)

template writeWith*(a: RwMonitor, body: untyped) =
  beginWrite(a)
  try:
    body
  finally:
    endWrite(a)

{.pop.}
