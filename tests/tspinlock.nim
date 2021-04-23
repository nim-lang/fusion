import sync, std / os

var
  data = 0
  thread: Thread[void]
  lock: SpinLock

proc threadFn =
  withLock lock:
    data = 1

proc sleep() =
  withLock lock:
    createThread(thread, threadFn)
    sleep(50)
    assert data == 0
  joinThread(thread)
  withLock lock:
    assert data == 1

sleep()
