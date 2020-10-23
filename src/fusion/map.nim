const
  # Minimal number of elements per node.
  # This should be a very small number, less than 10 for the most use cases.
  N = 10

type
  Entry[A, B] = object
    key: A
    val: B
    p: Node[A, B]

  Node[A, B] = ref object
    m: int  # number of elements
    p0: Node[A, B] # left-most pointer (nr. of pointers is always m+1)
    e: array[2*N, Entry[A, B]]

  CursorPosition[A, B] = tuple
    ## Index into map allowing access to either the key or the value.
    node: Node[A, B]
    entry: int

  Cursor[A, B] = seq[CursorPosition[A, B]]

  Map*[A, B] = object
    ## Generic container, consisting of key-value pairs, sorted by key
    ##
    ## `root` and `entries` are internal implementation details which cannot
    ## be directly accessed.
    ##
    ## For creating an empty Map, use `initMap proc<#initMap,int>`_.
    root: Node[A, B]
    entries: int # total number of entries in the tree

  MapRef*[A, B] = ref Map[A, B]  ## Ref version of `Map<#Map>`_.
    ##
    ## For creating a new empty MapRef, use `newMap proc
    ## <#newMap,int>`_.


template leq(a, b): bool = cmp(a, b) <= 0
template eq(a, b): bool = cmp(a, b) == 0

proc binarySearch[A, B](x: A; a: Node[A, B]): int {.inline.} =
  var
    l = 0
    r = a.m
    i: int
  while l < r:
    i = (l+r) div 2
    if leq(x, a.e[i].key):
      r = i
    else:
      l = i+1
  return r


proc initMap*[A, B](initialSize = 0): Map[A, B] =
  ## Creates a new empty Map.
  ##
  ## The `initialSize` parameter is there to be compatible with the
  ## hash table API, it has no effect on map
  ##
  ## See also:
  ## * `toMap proc<#toMap,openArray[]>`_
  ## * `newMap proc<#newMap,int>`_ for creating a `MapRef`
  runnableExamples:
    let
      a = initMap[int, string]()
      b = initMap[char, seq[int]]()
  result = Map[A, B](root: Node[A, B](m: 0, p0: nil), entries: 0)


proc `[]=`*[A, B](t: var Map[A, B]; key: A; val: B)

proc toMap*[A, B](pairs: openArray[(A, B)]): Map[A, B] =
  ## Creates a new Map which contains the given `pairs`.
  ##
  ## `pairs` is a container consisting of `(key, value)` tuples.
  ##
  ## See also:
  ## * `initMap proc<#initMap,int>`_
  ## * `newMap proc<#newMap,openArray[]>`_ for a `MapRef` version
  runnableExamples:
    let a = [('a', 5), ('b', 9)]
    let b = toMap(a)
    assert b == {'a': 5, 'b': 9}.toMap

  result = initMap[A, B]()
  for key, val in items(pairs):
    result[key] = val


template getHelper(a, x, ifFound, ifNotFound) {.dirty.} =
  while true:
    var r = binarySearch(x, a)
    if (r < a.m) and eq(x, a.e[r].key):
      return ifFound
    a = if r == 0: a.p0 else: a.e[r-1].p
    if a.isNil:
      return ifNotFound


proc getOrDefault*[A, B](t: Map[A, B]; x: A): B =
  ## Retrieves the value at `t[key]` if `key` is in `t`.
  ## Otherwise, the default initialization value for type `B` is returned
  ## (e.g. 0 for any integer type).
  ##
  ## See also:
  ## * `[] proc<#[],Map[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,Map[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,Map[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,Map[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toMap
    doAssert a.getOrDefault('a') == 5
    doAssert a.getOrDefault('z') == 0

  var a =
    if t.root.isNil: Node[A,B](m: 0, p0: nil)
    else: t.root
  getHelper(a, x, a.e[r].val, default(B))


proc getOrDefault*[A, B](t: Map[A, B];
                             x: A; default: B): B =
  ## Retrieves the value at `t[key]` if `key` is in `t`.
  ## Otherwise, `default` is returned.
  ##
  ## See also:
  ## * `[] proc<#[],Map[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,Map[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,Map[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,Map[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toMap
    doAssert a.getOrDefault('a', 99) == 5
    doAssert a.getOrDefault('z', 99) == 99

  var a =
    if t.root.isNil: Node[A,B](m: 0, p0: nil)
    else: t.root
  getHelper(a, x, a.e[r].val, default)


proc `[]`*[A, B](t: Map[A, B]; x: A): B =
  ## Retrieves the value at `t[key]`.
  ##
  ## If `key` is not in `t`, the `KeyError` exception is raised.
  ## One can check with `hasKey proc<#hasKey,Map[A,B],A>`_ whether
  ## the key exists.
  ##
  ## See also:
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  ## * `[]= proc<#[]=,Map[A,B],A,B>`_ for inserting a new
  ##   (key, value) pair in the map
  ## * `hasKey proc<#hasKey,Map[A,B],A>`_ for checking if a key is in
  ##   the map
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toMap
    doAssert a['a'] == 5
    doAssertRaises(KeyError):
      echo a['z']

  var a =
    if t.root.isNil: Node[A,B](m: 0, p0: nil)
    else: t.root
  while true:
    var r = binarySearch(x, a)
    if (r < a.m) and eq(x, a.e[r].key):
      return a.e[r].val
    a = if r == 0: a.p0 else: a.e[r-1].p
    if a.isNil:
      when compiles($key):
        raise newException(KeyError, "key not found: " & $key)
      else:
        raise newException(KeyError, "key not found")


proc `[]`*[A, B](t: var Map[A, B]; x: A): var B =
  ## Retrieves the value at `t[key]`. The value can be modified.
  ##
  ## If `key` is not in `t`, the `KeyError` exception is raised.
  ##
  ## See also:
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  ## * `[]= proc<#[]=,Map[A,B],A,B>`_ for inserting a new
  ##   (key, value) pair in the map
  ## * `hasKey proc<#hasKey,Map[A,B],A>`_ for checking if a key is in
  ##   the map
  var a =
    if t.root.isNil: Node[A,B](m: 0, p0: nil)
    else: t.root
  while true:
    var r = binarySearch(x, a)
    if (r < a.m) and eq(x, a.e[r].key):
      return a.e[r].val
    a = if r == 0: a.p0 else: a.e[r-1].p
    if a.isNil:
      when compiles($key):
        raise newException(KeyError, "key not found: " & $key)
      else:
        raise newException(KeyError, "key not found")


proc hasKey*[A, B](t: Map[A, B]; x: A): bool =
  ## Returns true if `key` is in the map `t`.
  ##
  ## See also:
  ## * `contains proc<#contains,Map[A,B],A>`_ for use with the `in` operator
  ## * `[] proc<#[],Map[A,B],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  var a =
    if t.root.isNil: Node[A,B](m: 0, p0: nil)
    else: t.root
  getHelper(a, x, true, false)


proc contains*[A, B](t: Map[A, B]; x: A): bool =
  ## Alias of `hasKey proc<#hasKey,Map[A,B],A>`_ for use with
  ## the `in` operator.
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toMap
    doAssert 'b' in a == true
    doAssert a.contains('z') == false

  return hasKey(t, x)


proc insertImpl[A, B](x: A; a: Node[A, B];
                      h: var bool; v: var Entry[A, B]): bool =
  # Search key x in B-tree with root a;
  # if found, change the value to the new one and return true.
  # Otherwise insert new item with key x.
  # If an entry is to be passed up, assign it to v.
  # h = "tree has become higher"

  result = true
  var u = v
  var r = binarySearch(x, a)

  if (r < a.m) and (a.e[r].key == x): # found
    a.e[r].val = v.val
    return false

  else: # item not on this page
    var b = if r == 0: a.p0 else: a.e[r-1].p

    if b.isNil: # not in tree, insert
      u.p = nil
      h = true
      u.key = x
    else:
      result = insertImpl(x, b, h, u)

    var i: int
    if h: # insert u to the left of a.e[r]
      if a.m < 2*N:
        h = false
        i = a.m
        while i > r:
          dec(i)
          a.e[i+1] = a.e[i]
        a.e[r] = u
        inc(a.m)
      else:
        new(b) # overflow; split a into a,b and assign the middle entry to v
        if r < N: # insert in left page a
          i = N-1
          v = a.e[i]
          while i > r:
            dec(i)
            a.e[i+1] = a.e[i]
          a.e[r] = u
          i = 0
          while i < N:
            b.e[i] = a.e[i+N]
            inc(i)
        else: # insert in right page b
          dec(r, N)
          i = 0
          if r == 0:
            v = u
          else:
            v = a.e[N]
            while i < r-1:
              b.e[i] = a.e[i+N+1]
              inc(i)
            b.e[i] = u
            inc(i)
          while i < N:
            b.e[i] = a.e[i+N]
            inc(i)
        a.m = N
        b.m = N
        b.p0 = v.p
        v.p = b


template insertHelper(t, key, val) =
  var u = Entry[A, B](key: key, val: val)
  var h = false
  var wasAdded = insertImpl(key, t.root, h, u)
  if wasAdded:
    inc(t.entries)
  if h: # the previous root had to be splitted, create a new one
    var q = t.root
    new(t.root)
    t.root.m = 1
    t.root.p0 = q
    t.root.e[0] = u


proc `[]=`*[A, B](t: var Map[A, B]; key: A; val: B) =
  ## Inserts a `(key, value)` pair into `t`.
  ##
  ## See also:
  ## * `[] proc<#[],Map[A,B],A>`_ for retrieving a value of a key
  ## * `hasKeyOrPut proc<#hasKeyOrPut,Map[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,Map[A,B],A,B>`_
  ## * `del proc<#del,Map[A,B],A>`_ for removing a key from the map
  runnableExamples:
    var a = initMap[char, int]()
    a['x'] = 7
    a['y'] = 33
    doAssert a == {'x': 7, 'y': 33}.toMap

  if t.root.isNil: t = initMap[A, B]()
  insertHelper(t, key, val)


proc hasKeyOrPut*[A, B](t: var Map[A, B]; key: A; val: B): bool =
  ## Returns true if `key` is in the map, otherwise inserts `value`.
  ##
  ## See also:
  ## * `hasKey proc<#hasKey,Map[A,B],A>`_
  ## * `[] proc<#[],Map[A,B],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    var a = {'a': 5, 'b': 9}.toMap
    if a.hasKeyOrPut('a', 50):
      a['a'] = 99
    if a.hasKeyOrPut('z', 50):
      a['z'] = 99
    doAssert a == {'a': 99, 'b': 9, 'z': 50}.toMap

  if t.root.isNil: t = initMap[A, B]()
  if hasKey(t, key):
    result = true
  else:
    insertHelper(t, key, val)
    result = false

proc mgetOrPut*[A, B](t: var Map[A, B], key: A, val: B): var B =
  ## Retrieves value at ``t[key]`` or puts ``val`` if not present, either way
  ## returning a value which can be modified.
  ##
  ## See also:
  ## * `[] proc<#[],Map[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,Map[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,Map[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,Map[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    var a = {'a': 5, 'b': 9}.toMap
    doAssert a.mgetOrPut('a', 99) == 5
    doAssert a.mgetOrPut('z', 99) == 99
    doAssert a == {'a': 5, 'b': 9, 'z': 99}.toMap

  # XXX: does this work correctly?
  if key notin t:
    t[key] = val
  return t[key]


proc underflowImpl[A, B](c, a: Node[A, B]; s: int; h: var bool) =
  # a = underflowing page,
  # c = ancestor page,
  # s = index of deleted entry in c
  var
    s = s
    b: Node[A, B]
    i, k: int

  if s < c.m: # b = page to the *right* of a
    b = c.e[s].p
    k = (b.m - N + 1) div 2 # k = number of surplus items available on page b
    a.e[N-1] = c.e[s]
    a.e[N-1].p = b.p0

    if k > 0: # balance by moving k-1 items from b to a
      while i < k-1:
        a.e[i+N] = b.e[i]
        inc(i)
      c.e[s] = b.e[k-1]
      b.p0 = c.e[s].p
      c.e[s].p = b
      dec(b.m, k)
      i = 0
      while i < b.m:
        b.e[i] = b.e[i+k]
        inc(i)
      a.m = N-1+k
      h = false
    else: # no surplus items in b: merge pages a and b, discard *b*
      i = 0
      while i < N:
        a.e[i+N] = b.e[i]
        inc(i)
      i = s
      dec(c.m)
      while i < c.m:
        c.e[i] = c.e[i+1]
        inc(i)
      a.m = 2*N
      h = c.m < N

  else: # b = page to the *left* of a
    dec(s)
    b = if s == 0: c.p0 else: c.e[s-1].p
    k = (b.m - N + 1) div 2 # k = number of surplus items available on page b

    if k > 0:
      i = N-1
      while i > 0:
        dec(i)
        a.e[i+k] = a.e[i]
      i = k-1
      a.e[i] = c.e[s]
      a.e[i].p = a.p0
      # move k-1 items from b to a, and one to c
      dec(b.m, k)
      while i > 0:
        dec(i)
        a.e[i] = b.e[i+b.m+1]
      c.e[s] = b.e[b.m]
      a.p0 = c.e[s].p
      c.e[s].p = a
      a.m = N-1 + k
      h = false
    else: # no surplus items in b: merge pages a and b, discard *a*
      c.e[s].p = a.p0
      b.e[N] = c.e[s]
      i = 0
      while i < N-1:
        b.e[i+N+1] = a.e[i]
        inc(i)
      b.m = 2*N
      dec(c.m)
      h = c.m < N



proc deleteImpl[A, B](x: A; a: Node[A, B]; h: var bool): bool =
  # search and delete key x in B-tree a;
  # if a page underflow arises, balance with adjacent page or merge;
  # h = "page a is undersize"
  if a.isNil: # if the key wasn't in the map
    return false

  result = true
  var r = binarySearch(x, a)
  var q = if r == 0: a.p0 else: a.e[r-1].p

  proc del[A, B](p, a: Node[A, B]; h: var bool) =
    var
      k: int
      q: Node[A, B]
    k = p.m-1
    q = p.e[k].p
    if q != nil:
      del(q, a, h)
      if h:
        underflowImpl(p, q, p.m, h)
    else:
      p.e[k].p = a.e[r].p
      a.e[r] = p.e[k]
      dec(p.m)
      h = p.m < N

  var i: int
  if (r < a.m) and (a.e[r].key == x): # found
    if q.isNil: # a is leaf page
      dec(a.m)
      h = a.m < N
      i = r
      while i < a.m:
        a.e[i] = a.e[i+1]
        inc(i)
    else:
      del(q, a, h)
      if h:
        underflowImpl(a, q, r, h)
  else:
    result = deleteImpl(x, q, h)
    if h:
      underflowImpl(a, q, r, h)


proc delHelper[A, B](t: var Map[A, B]; key: A) =
  var h = false
  var wasDeleted = deleteImpl(key, t.root, h)
  if wasDeleted:
    dec(t.entries)
  if h: # the previous root is gone, appoint a new one
    if t.root.m == 0:
      t.root = t.root.p0

proc del*[A, B](t: var Map[A, B]; key: A) =
  ## Deletes `key` from map `t`. Does nothing if the key does not exist.
  ##
  ## See also:
  ## * `pop proc<#pop,Map[A,B],A,B>`_
  ## * `clear proc<#clear,Map[A,B]>`_ to empty the whole map
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.toMap
    a.del('a')
    doAssert a == {'b': 9, 'c': 13}.toMap
    a.del('z')
    doAssert a == {'b': 9, 'c': 13}.toMap

  if t.root.isNil: t = initMap[A, B]()
  delHelper(t, key)

proc pop*[A, B](t: var Map[A, B], key: A, val: var B): bool =
  ## Deletes the `key` from the map.
  ## Returns `true`, if the `key` existed, and sets `val` to the
  ## mapping of the key. Otherwise, returns `false`, and the `val` is
  ## unchanged.
  ##
  ## See also:
  ## * `del proc<#del,Map[A,B],A>`_
  ## * `clear proc<#clear,Map[A,B]>`_ to empty the whole table
  runnableExamples:
    var
      a = {'a': 5, 'b': 9, 'c': 13}.toMap
      i: int
    doAssert a.pop('b', i) == true
    doAssert a == {'a': 5, 'c': 13}.toMap
    doAssert i == 9
    i = 0
    doAssert a.pop('z', i) == false
    doAssert a == {'a': 5, 'c': 13}.toMap
    doAssert i == 0

  if t.root.isNil: t = initMap[A, B]()
  result = t.hasKey(key)
  if result:
    val = t[key]
    delHelper(t, key)

proc take*[A, B](t: var Map[A, B];
                     key: A; val: var B): bool =
  ## Alias for:
  ## * `pop proc<#pop,Map[A,B],A,B>`_
  pop(t, key, val)


proc clear*[A, B](t: var Map[A, B]) =
  ## Resets the map so that it is empty.
  ##
  ## See also:
  ## * `del proc<#del,Map[A,B],A>`_
  ## * `pop proc<#pop,Map[A,B],A,B>`_
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.toMap
    doAssert len(a) == 3
    clear(a)
    doAssert len(a) == 0

  # XXX: can we simplify it like this?
  t = initMap[A, B]()

proc len*[A, B](t: Map[A, B]): int {.inline.} =
  ## Returns the number of keys in ``t``.
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toMap
    doAssert len(a) == 2

  t.entries

proc key[A, B](position: CursorPosition[A, B]): A =
  ## Return the key for a given cursor position.
  position.node.e[position.entry].key

proc val[A, B](position: CursorPosition[A, B]): B =
  ## Return the value for a given cursor position.
  position.node.e[position.entry].val

proc mval[A, B](position: CursorPosition[A, B]): var B =
  ## Returns a reference to the value for a given cursor position.
  position.node.e[position.entry].val

proc search[A, B](b: Map[A, B], key: A): Cursor[A, B] =
  ## Calculates the cursor pointing to the given key.
  var a = b.root
  while not a.isNil:
    var r = binarySearch(key, a)
    if r < a.m:
      result.add((a, r))
      if eq(key, a.e[r].key):
        break
    a = if r == 0: a.p0 else: a.e[r-1].p
  # add a dummy entry for first next call
  result.add((nil, 0))

proc current[A, B](cursor: Cursor[A, B]): CursorPosition[A, B] =
  ## Returns the current position of a cursor.
  ## This call is only valid if cursor.next previously returned true.
  cursor[^1]

proc next[A, B](cursor: var Cursor[A, B]): bool =
  ## Moves the cursor forward returning true if cursor.current is now valid.
  ## Never call current after next returns false.
  var (node, oldEntry) = cursor.pop()
  if not node.isNil:
    var newEntry = oldEntry + 1
    if newEntry < node.m:
        cursor.add((node, newEntry))
    var child = node.e[oldEntry].p
    if not child.isNil:
      while not child.isNil:
        cursor.add((child, 0))
        child = child.p0
  return cursor.len > 0 and cursor.current.node.m > 0

proc cursorFromStart[A, B](b: Map[A, B]): Cursor[A, B] =
  result = @[]
  var a = b.root
  while not a.isNil:
    result.add((a, 0))
    a = a.p0
  result.add((nil, 0))

iterator entries[A, B](b: Map[A, B]): CursorPosition[A, B] =
  var cursor = b.cursorFromStart
  while cursor.next:
    yield cursor.current


iterator entriesFrom[A, B](b: Map[A, B], fromKey: A): CursorPosition[A, B] =
  # Iterates the sorted map from the given key to the end.
  var cursor = b.search(fromKey)
  while cursor.next:
    yield cursor.current

iterator entriesBetween[A, B](b: Map[A, B], fromKey: A, toKey: A): CursorPosition[A, B] =
  # Iterates the sorted map from fromKey to toKey inclusive.
  var cursor = b.search(fromKey)
  while cursor.next:
    let position = cursor.current
    if not leq(position.key, toKey):
      break
    yield position


iterator keys*[A, B](t: Map[A, B]): A =
  ## Iterates over all the keys in the map `t`.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,Map[A,B]>`_
  ## * `values iterator<#values.i,Map[A,B]>`_
  runnableExamples:
    var a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.toMap
    for k in a.keys:
      a[k].add(99)
    doAssert a == {'e': @[2, 4, 6, 8, 99], 'o': @[1, 5, 7, 9, 99]}.toMap

  for e in entries(t):
    yield e.key

iterator keysFrom*[A, B](b: Map[A, B], fromKey: A): A =
  ## Iterates over keys in the map from `fromKey` to the end.
  for e in entriesFrom(b, fromKey):
    yield e.key

iterator keysBetween*[A, B](b: Map[A, B], fromKey: A, toKey: A): A =
  ## Iterates over keys in the map from `fromKey` to `toKey` inclusive.
  for e in entriesBetween(b, fromKey, toKey):
    yield e.key


iterator values*[A, B](t: Map[A, B]): B =
  ## Iterates over all the values in the map `t`.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,Map[A,B]>`_
  ## * `keys iterator<#keys.i,Map[A,B]>`_
  ## * `mvalues iterator<#mvalues.i,Map[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.toMap
    for v in a.values:
      doAssert v.len == 4

  for e in entries(t):
    yield e.val

iterator mvalues*[A, B](t: var Map[A, B]): var B =
  ## Iterates over all the values in the map `t`.
  ## The values can be modified.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,Map[A,B]>`_
  ## * `values iterator<#values.i,Map[A,B]>`_
  runnableExamples:
    var a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.toMap
    for v in a.mvalues:
      v.add(99)
    doAssert a == {'e': @[2, 4, 6, 8, 99], 'o': @[1, 5, 7, 9, 99]}.toMap

  for e in entries(t):
    yield e.mval

iterator valuesFrom*[A, B](b: Map[A, B], fromKey: A): B =
  ## Iterates over the values in the map from the given key to the end.
  for e in entriesFrom(b, fromKey):
    yield e.val

iterator valuesBetween*[A, B](b: Map[A, B], fromKey: A, toKey: A): B =
  ## Iterates over the values in the map from `fromKey` to `toKey` inclusive.
  for e in entriesBetween(b, fromKey, toKey):
    yield e.val


iterator pairs*[A, B](t: Map[A, B]): (A, B) =
  ## Iterates over all `(key, value)` pairs in the map `t`.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,Map[A,B]>`_
  ## * `keys iterator<#keys.i,Map[A,B]>`_
  ## * `values iterator<#values.i,Map[A,B]>`_
  ##
  ## **Examples:**
  ##
  ## .. code-block::
  ##   let a = {
  ##     'o': [1, 5, 7, 9],
  ##     'e': [2, 4, 6, 8]
  ##     }.toMap
  ##
  ##   for k, v in a.pairs:
  ##     echo "key: ", k
  ##     echo "value: ", v
  ##
  ##   # key: e
  ##   # value: [2, 4, 6, 8]
  ##   # key: o
  ##   # value: [1, 5, 7, 9]
  for e in entries(t):
    yield (e.key, e.val)

iterator mpairs*[A, B](t: var Map[A, B]): (A, var B) =
  ## Iterates over all `(key, value)` pairs in the map `t`.
  ## The values can be modified.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,Map[A,B]>`_
  ## * `mvalues iterator<#mvalues.i,Map[A,B]>`_
  runnableExamples:
    var a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.toMap
    for k, v in a.mpairs:
      v.add(v[0] + 10)
    doAssert a == {'e': @[2, 4, 6, 8, 12], 'o': @[1, 5, 7, 9, 11]}.toMap

  for e in entries(t):
    yield (e.key, e.mval)


iterator pairsFrom*[A, B](b: Map[A, B], fromKey: A): tuple[key: A, val: B] =
  ## Iterates over `(key, value)` pairs in the map from the given key to the end.
  for e in entriesFrom(b, fromKey):
    yield (e.key, e.val)

iterator pairsBetween*[A, B](b: Map[A, B], fromKey: A, toKey: A): tuple[key: A, val: B] =
  ## Iterates over `(key, value)` pairs in the map from `fromKey` to `toKey` inclusive.
  for e in entriesBetween(b, fromKey, toKey):
    yield (e.key, e.val)


proc `$`*[A, B](t: Map[A, B]): string =
  ## The ``$`` operator for maps. Used internally when calling `echo`
  ## on a map.
  if t.entries == 0:
    result = "{:}"
  else:
    result = "{"
    for (k, v) in pairs(t):
      if result.len > 1: result.add(", ")
      result.addQuoted(k)
      result.add(": ")
      result.addQuoted(v)
    result.add("}")

proc `==`*[A, B](a, b: Map[A, B]): bool =
  ## The `==` operator for Maps.
  ##
  ## Returns `true` if the content of both maps contains the same
  ## key-value pairs. Insert order does not matter.
  runnableExamples:
    let
      a = {'a': 5, 'b': 9, 'c': 13}.toMap
      b = {'b': 9, 'c': 13, 'a': 5}.toMap
    doAssert a == b

  if a.root.isNil and b.root.isNil:
    return true
  if a.entries == b.entries:
    for k, v in a:
      if not b.hasKey(k): return false
      if b.getOrDefault(k) != v: return false
    return true





# -------------------------------------------------------------------
# ---------------------------- MapRef -----------------------------
# -------------------------------------------------------------------


proc newMap*[A, B](): <//>MapRef[A, B] =
  ## Creates a new ref map that is empty.
  ##
  ## ``initialSize`` must be a power of two (default: 64).
  ## If you need to accept runtime values for this you could use the
  ## `nextPowerOfTwo proc<math.html#nextPowerOfTwo,int>`_ from the
  ## `math module<math.html>`_ or the `rightSize proc<#rightSize,Natural>`_
  ## from this module.
  ##
  ## See also:
  ## * `newMap proc<#newMap,openArray[]>`_ for creating a `MapRef`
  ##   from a collection of `(key, value)` pairs
  ## * `initMap proc<#initMap,int>`_ for creating a `Map`
  runnableExamples:
    let
      a = newMap[int, string]()
      b = newMap[char, seq[int]]()

  new(result)
  result[] = initMap[A, B]()

proc newMap*[A, B](pairs: openArray[(A, B)]): <//>MapRef[A, B] =
  ## Creates a new ref map that contains the given ``pairs``.
  ##
  ## ``pairs`` is a container consisting of ``(key, value)`` tuples.
  ##
  ## See also:
  ## * `newMap proc<#newMap,int>`_
  ## * `toMap proc<#toMap,openArray[]>`_ for a `Map` version
  runnableExamples:
    let a = [('a', 5), ('b', 9)]
    let b = newMap(a)
    assert b == {'a': 5, 'b': 9}.newMap

  new(result)
  result[] = toMap[A, B](pairs)

proc newMapFrom*[A, B, C](collection: A, index: proc(x: B): C): <//>MapRef[C, B] =
  ## Index the collection with the proc provided.
  # TODO: As soon as supported, change collection: A to collection: A[B]
  result = newMap[C, B]()
  for item in collection:
    result[index(item)] = item


proc `[]`*[A, B](t: MapRef[A, B], key: A): var B =
  ## Retrieves the value at ``t[key]``.
  ##
  ## If ``key`` is not in ``t``, the  ``KeyError`` exception is raised.
  ## One can check with `hasKey proc<#hasKey,MapRef[A,B],A>`_ whether
  ## the key exists.
  ##
  ## See also:
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  ## * `[]= proc<#[]=,MapRef[A,B],A,B>`_ for inserting a new
  ##   (key, value) pair in the map
  ## * `hasKey proc<#hasKey,MapRef[A,B],A>`_ for checking if a key is in
  ##   the map
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newMap
    doAssert a['a'] == 5
    doAssertRaises(KeyError):
      echo a['z']

  result = t[][key]

proc `[]=`*[A, B](t: MapRef[A, B], key: A, val: B) =
  ## Inserts a ``(key, value)`` pair into ``t``.
  ##
  ## See also:
  ## * `[] proc<#[],MapRef[A,B],A>`_ for retrieving a value of a key
  ## * `hasKeyOrPut proc<#hasKeyOrPut,MapRef[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,MapRef[A,B],A,B>`_
  ## * `del proc<#del,MapRef[A,B],A>`_ for removing a key from the map
  runnableExamples:
    var a = newMap[char, int]()
    a['x'] = 7
    a['y'] = 33
    doAssert a == {'x': 7, 'y': 33}.newMap

  t[][key] = val

proc hasKey*[A, B](t: MapRef[A, B], key: A): bool =
  ## Returns true if ``key`` is in the map ``t``.
  ##
  ## See also:
  ## * `contains proc<#contains,MapRef[A,B],A>`_ for use with the `in`
  ##   operator
  ## * `[] proc<#[],MapRef[A,B],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newMap
    doAssert a.hasKey('a') == true
    doAssert a.hasKey('z') == false

  result = t[].hasKey(key)


proc contains*[A, B](t: MapRef[A, B], key: A): bool =
  ## Alias of `hasKey proc<#hasKey,MapRef[A,B],A>`_ for use with
  ## the ``in`` operator.
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newMap
    doAssert 'b' in a == true
    doAssert a.contains('z') == false

  return hasKey[A, B](t, key)

proc hasKeyOrPut*[A, B](t: var MapRef[A, B], key: A, val: B): bool =
  ## Returns true if ``key`` is in the map, otherwise inserts ``value``.
  ##
  ## See also:
  ## * `hasKey proc<#hasKey,MapRef[A,B],A>`_
  ## * `[] proc<#[],MapRef[A,B],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    var a = {'a': 5, 'b': 9}.newMap
    if a.hasKeyOrPut('a', 50):
      a['a'] = 99
    if a.hasKeyOrPut('z', 50):
      a['z'] = 99
    doAssert a == {'a': 99, 'b': 9, 'z': 50}.newMap

  t[].hasKeyOrPut(key, val)

proc getOrDefault*[A, B](t: MapRef[A, B], key: A): B =
  ## Retrieves the value at ``t[key]`` if ``key`` is in ``t``. Otherwise, the
  ## default initialization value for type ``B`` is returned (e.g. 0 for any
  ## integer type).
  ##
  ## See also:
  ## * `[] proc<#[],MapRef[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,MapRef[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,MapRef[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,MapRef[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newMap
    doAssert a.getOrDefault('a') == 5
    doAssert a.getOrDefault('z') == 0

  getOrDefault(t[], key)

proc getOrDefault*[A, B](t: MapRef[A, B], key: A, default: B): B =
  ## Retrieves the value at ``t[key]`` if ``key`` is in ``t``.
  ## Otherwise, ``default`` is returned.
  ##
  ## See also:
  ## * `[] proc<#[],MapRef[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,MapRef[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,MapRef[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,MapRef[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newMap
    doAssert a.getOrDefault('a', 99) == 5
    doAssert a.getOrDefault('z', 99) == 99

  getOrDefault(t[], key, default)

proc mgetOrPut*[A, B](t: MapRef[A, B], key: A, val: B): var B =
  ## Retrieves value at ``t[key]`` or puts ``val`` if not present, either way
  ## returning a value which can be modified.
  ##
  ## See also:
  ## * `[] proc<#[],MapRef[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,MapRef[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,MapRef[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,MapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    var a = {'a': 5, 'b': 9}.newMap
    doAssert a.mgetOrPut('a', 99) == 5
    doAssert a.mgetOrPut('z', 99) == 99
    doAssert a == {'a': 5, 'b': 9, 'z': 99}.newMap

  t[].mgetOrPut(key, val)


proc len*[A, B](t: MapRef[A, B]): int =
  ## Returns the number of keys in ``t``.
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newMap
    doAssert len(a) == 2

  result = t.entries


proc del*[A, B](t: MapRef[A, B], key: A) =
  ## Deletes ``key`` from map ``t``. Does nothing if the key does not exist.
  ##
  ## **If duplicate keys were added, this may need to be called multiple times.**
  ##
  ## See also:
  ## * `pop proc<#pop,MapRef[A,B],A,B>`_
  ## * `clear proc<#clear,MapRef[A,B]>`_ to empty the whole map
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.newMap
    a.del('a')
    doAssert a == {'b': 9, 'c': 13}.newMap
    a.del('z')
    doAssert a == {'b': 9, 'c': 13}.newMap

  t[].del(key)

proc pop*[A, B](t: MapRef[A, B], key: A, val: var B): bool =
  ## Deletes the ``key`` from the map.
  ## Returns ``true``, if the ``key`` existed, and sets ``val`` to the
  ## mapping of the key. Otherwise, returns ``false``, and the ``val`` is
  ## unchanged.
  ##
  ## **If duplicate keys were added, this may need to be called multiple times.**
  ##
  ## See also:
  ## * `del proc<#del,MapRef[A,B],A>`_
  ## * `clear proc<#clear,MapRef[A,B]>`_ to empty the whole map
  runnableExamples:
    var
      a = {'a': 5, 'b': 9, 'c': 13}.newMap
      i: int
    doAssert a.pop('b', i) == true
    doAssert a == {'a': 5, 'c': 13}.newMap
    doAssert i == 9
    i = 0
    doAssert a.pop('z', i) == false
    doAssert a == {'a': 5, 'c': 13}.newMap
    doAssert i == 0

  result = t[].pop(key, val)


proc take*[A, B](t: MapRef[A, B], key: A, val: var B): bool {.inline.} =
  ## Alias for:
  ## * `pop proc<#pop,MapRef[A,B],A,B>`_
  pop(t, key, val)


proc clear*[A, B](t: MapRef[A, B]) =
  ## Resets the map so that it is empty.
  ##
  ## See also:
  ## * `del proc<#del,Map[A,B],A>`_
  ## * `pop proc<#pop,Map[A,B],A,B>`_
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.newMap
    doAssert len(a) == 3
    clear(a)
    doAssert len(a) == 0

  # XXX: can we simplify it like this?
  t[] = initMap[A, B]()

proc `$`*[A, B](t: MapRef[A, B]): string =
  ## The ``$`` operator for maps. Used internally when calling `echo`
  ## on a map.
  `$`(t[])


proc `==`*[A, B](s, t: MapRef[A, B]): bool =
  ## The ``==`` operator for maps. Returns ``true`` if either both maps
  ## are ``nil``, or neither is ``nil`` and the content of both maps contains the
  ## same key-value pairs. Insert order does not matter.
  runnableExamples:
    let
      a = {'a': 5, 'b': 9, 'c': 13}.newMap
      b = {'b': 9, 'c': 13, 'a': 5}.newMap
    doAssert a == b

  if isNil(s): result = isNil(t)
  elif isNil(t): result = false
  else: result = s[] == t[]


iterator keys*[A, B](t: MapRef[A, B]): A =
  ## Iterates over any key in the map ``t``.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,MapRef[A,B]>`_
  ## * `values iterator<#values.i,MapRef[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.newMap
    for k in a.keys:
      a[k].add(99)
    doAssert a == {'e': @[2, 4, 6, 8, 99], 'o': @[1, 5, 7, 9, 99]}.newMap

  for k in keys(t[]):
    yield k

iterator values*[A, B](t: MapRef[A, B]): B =
  ## Iterates over any value in the map ``t``.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,MapRef[A,B]>`_
  ## * `keys iterator<#keys.i,MapRef[A,B]>`_
  ## * `mvalues iterator<#mvalues.i,MapRef[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.newMap
    for v in a.values:
      doAssert v.len == 4

  for v in values(t[]):
    yield v

iterator mvalues*[A, B](t: MapRef[A, B]): var B =
  ## Iterates over any value in the map ``t``. The values can be modified.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,MapRef[A,B]>`_
  ## * `values iterator<#values.i,MapRef[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.newMap
    for v in a.mvalues:
      v.add(99)
    doAssert a == {'e': @[2, 4, 6, 8, 99], 'o': @[1, 5, 7, 9, 99]}.newMap

  for v in mvalues(t[]):
    yield v

iterator pairs*[A, B](t: MapRef[A, B]): (A, B) =
  ## Iterates over any ``(key, value)`` pair in the map ``t``.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,MapRef[A,B]>`_
  ## * `keys iterator<#keys.i,MapRef[A,B]>`_
  ## * `values iterator<#values.i,MapRef[A,B]>`_
  ##
  ## **Examples:**
  ##
  ## .. code-block::
  ##   let a = {
  ##     'o': [1, 5, 7, 9],
  ##     'e': [2, 4, 6, 8]
  ##     }.newMap
  ##
  ##   for k, v in a.pairs:
  ##     echo "key: ", k
  ##     echo "value: ", v
  ##
  ##   # key: e
  ##   # value: [2, 4, 6, 8]
  ##   # key: o
  ##   # value: [1, 5, 7, 9]
  for p in pairs(t[]):
    yield p

iterator mpairs*[A, B](t: MapRef[A, B]): (A, var B) =
  ## Iterates over any ``(key, value)`` pair in the map ``t``. The values
  ## can be modified.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,MapRef[A,B]>`_
  ## * `mvalues iterator<#mvalues.i,MapRef[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.newMap
    for k, v in a.mpairs:
      v.add(v[0] + 10)
    doAssert a == {'e': @[2, 4, 6, 8, 12], 'o': @[1, 5, 7, 9, 11]}.newMap

  for (k, v) in mpairs(t[]):
    yield (k, v)





# ---------------------------------------------------------------------------
# ------------------------------ OrderedMap -------------------------------
# ---------------------------------------------------------------------------



const SmallLimit = 2 # XXX: small for testing purposes, should be a bit higher

type
  OrderedMap*[A, B] = object
    ## Map that remembers insertion order.
    ##
    ## For creating an empty OrderedMap, use `initOrderedMap proc
    ## <#initOrderedMap,int>`_.
    data: seq[(A, B)]
    byKey: Map[A, int] # int --> position of data

  OrderedMapRef*[A, B] = ref OrderedMap[A, B] ## Ref version of
    ## `OrderedMap<#OrderedMap>`_.
    ##
    ## For creating a new empty OrderedMapRef, use `newOrderedMap proc
    ## <#newOrderedMap,int>`_.


template isSmall(t): untyped =
  t.data.len < SmallLimit and t.byKey.len == 0


proc findInData[A, B](data: seq[(A, B)]; k: A): int =
  for i in 0 ..< data.len:
    if data[i][0] == k: return i
  return -1

proc populateByKey[A, B](t: var OrderedMap[A, B]) =
  for i in 0 ..< t.data.len:
    var k = t.data[i][0]
    t.byKey[k] = i

proc putImpl[A, B](t: var OrderedMap[A, B]; k: A; v: B) =
  var keyIndex: int
  if isSmall(t):
    keyIndex = findInData(t.data, k)
    if keyIndex < 0:
      t.data.add((k, v))
      if t.data.len == Smalllimit:
        populateByKey(t)
    else:
      t.data[keyIndex] = (k, v)
  else:
    keyIndex = getOrDefault(t.byKey, k, -1)
    if keyIndex < 0:
      t.data.add((k, v))
      t.byKey[k] = t.data.high
    else:
      t.data[keyIndex] = (k, v)
      t.byKey[k] = keyIndex

template get(t, key): untyped =
  if isSmall(t):
    for i in 0 ..< t.data.len:
      if t.data[i][0] == key:
        return t.data[i][1]
    when compiles($key):
      raise newException(KeyError, "key not found: " & $key)
    else:
      raise newException(KeyError, "key not found")
  else:
    var keyIndex = t.byKey[key] # this will raise an exception if not found
    return (t.data[keyIndex])[1]


proc initOrderedMap*[A, B](initialSize = 64): OrderedMap[A, B] =
  ## Creates a new ordered map that is empty.
  ##
  ## Starting from Nim v0.20, maps are initialized by default and it is
  ## not necessary to call this function explicitly.
  ##
  ## See also:
  ## * `toOrderedMap proc<#toOrderedMap,openArray[]>`_
  ## * `newOrderedMap proc<#newOrderedMap,int>`_ for creating an
  ##   `OrderedMapRef`
  runnableExamples:
    let
      a = initOrderedMap[int, string]()
      b = initOrderedMap[char, seq[int]]()
  result = OrderedMap[A, B](data: newSeqOfCap[(A, B)](initialSize),
                              byKey: initMap[A, int]())

proc `[]=`*[A, B](t: var OrderedMap[A, B]; k: A; v: B) =
  ## Inserts a ``(key, value)`` pair into ``t``.
  ##
  ## See also:
  ## * `[] proc<#[],OrderedMap[A,B],A>`_ for retrieving a value of a key
  ## * `hasKeyOrPut proc<#hasKeyOrPut,OrderedMap[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,OrderedMap[A,B],A,B>`_
  ## * `del proc<#del,OrderedMap[A,B],A>`_ for removing a key from the map
  runnableExamples:
    var a = initOrderedMap[char, int]()
    a['x'] = 7
    a['y'] = 33
    doAssert a == {'x': 7, 'y': 33}.toOrderedMap

  putImpl(t, k, v)


proc toOrderedMap*[A, B](pairs: openArray[(A, B)]): OrderedMap[A, B] =
  ## Creates a new ordered map that contains the given ``pairs``.
  ##
  ## ``pairs`` is a container consisting of ``(key, value)`` tuples.
  ##
  ## See also:
  ## * `initOrderedMap proc<#initOrderedMap,int>`_
  ## * `newOrderedMap proc<#newOrderedMap,openArray[]>`_ for an
  ##   `OrderedMapRef` version
  runnableExamples:
    let a = [('a', 5), ('b', 9)]
    let b = toOrderedMap(a)
    assert b == {'a': 5, 'b': 9}.toOrderedMap

  for (key, val) in items(pairs):
    result[key] = val

proc `[]`*[A, B](t: OrderedMap[A, B], key: A): B =
  ## Retrieves the value at ``t[key]``.
  ##
  ## If ``key`` is not in ``t``, the  ``KeyError`` exception is raised.
  ## One can check with `hasKey proc<#hasKey,OrderedMap[A,B],A>`_ whether
  ## the key exists.
  ##
  ## See also:
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  ## * `[]= proc<#[]=,OrderedMap[A,B],A,B>`_ for inserting a new
  ##   (key, value) pair in the map
  ## * `hasKey proc<#hasKey,OrderedMap[A,B],A>`_ for checking if a
  ##   key is in the map
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toOrderedMap
    doAssert a['a'] == 5
    doAssertRaises(KeyError):
      echo a['z']

  get(t, key)

proc `[]`*[A, B](t: var OrderedMap[A, B], key: A): var B =
  ## Retrieves the value at ``t[key]``. The value can be modified.
  ##
  ## If ``key`` is not in ``t``, the ``KeyError`` exception is raised.
  ##
  ## See also:
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  ## * `[]= proc<#[]=,OrderedMap[A,B],A,B>`_ for inserting a new
  ##   (key, value) pair in the map
  ## * `hasKey proc<#hasKey,OrderedMap[A,B],A>`_ for checking if a
  ##   key is in the map
  get(t, key)

proc hasKey*[A, B](t: OrderedMap[A, B], key: A): bool =
  ## Returns true if ``key`` is in the map ``t``.
  ##
  ## See also:
  ## * `contains proc<#contains,OrderedMap[A,B],A>`_ for use with the `in`
  ##   operator
  ## * `[] proc<#[],OrderedMap[A,B],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toOrderedMap
    doAssert a.hasKey('a') == true
    doAssert a.hasKey('z') == false

  if isSmall(t):
    for (k, _) in t.data:
      if k == key:
        return true
  else:
    return key in t.byKey

proc contains*[A, B](t: OrderedMap[A, B], key: A): bool =
  ## Alias of `hasKey proc<#hasKey,OrderedMap[A,B],A>`_ for use with
  ## the ``in`` operator.
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toOrderedMap
    doAssert 'b' in a == true
    doAssert a.contains('z') == false

  return hasKey[A, B](t, key)

proc hasKeyOrPut*[A, B](t: var OrderedMap[A, B], key: A, val: B): bool =
  ## Returns true if ``key`` is in the map, otherwise inserts ``value``.
  ##
  ## See also:
  ## * `hasKey proc<#hasKey,OrderedMap[A,B],A>`_
  ## * `[] proc<#[],OrderedMap[A,B],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    var a = {'a': 5, 'b': 9}.toOrderedMap
    if a.hasKeyOrPut('a', 50):
      a['a'] = 99
    if a.hasKeyOrPut('z', 50):
      a['z'] = 99
    doAssert a == {'a': 99, 'b': 9, 'z': 50}.toOrderedMap

  result = key in t
  if not result:
    t[key] = val

proc getOrDefault*[A, B](t: OrderedMap[A, B], key: A): B =
  ## Retrieves the value at ``t[key]`` if ``key`` is in ``t``. Otherwise, the
  ## default initialization value for type ``B`` is returned (e.g. 0 for any
  ## integer type).
  ##
  ## See also:
  ## * `[] proc<#[],OrderedMap[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,OrderedMap[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,OrderedMap[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,OrderedMap[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toOrderedMap
    doAssert a.getOrDefault('a') == 5
    doAssert a.getOrDefault('z') == 0

  if key in t:
    return t[key]
  else:
    return default(B)

proc getOrDefault*[A, B](t: OrderedMap[A, B], key: A, default: B): B =
  ## Retrieves the value at ``t[key]`` if ``key`` is in ``t``.
  ## Otherwise, ``default`` is returned.
  ##
  ## See also:
  ## * `[] proc<#[],OrderedMap[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,OrderedMap[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,OrderedMap[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,OrderedMap[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toOrderedMap
    doAssert a.getOrDefault('a', 99) == 5
    doAssert a.getOrDefault('z', 99) == 99

  if key in t:
    return t[key]
  else:
    return default

proc mgetOrPut*[A, B](t: var OrderedMap[A, B], key: A, val: B): var B =
  ## Retrieves value at ``t[key]`` or puts ``val`` if not present, either way
  ## returning a value which can be modified.
  ##
  ## See also:
  ## * `[] proc<#[],OrderedMap[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,OrderedMap[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,OrderedMap[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,OrderedMap[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    var a = {'a': 5, 'b': 9}.toOrderedMap
    doAssert a.mgetOrPut('a', 99) == 5
    doAssert a.mgetOrPut('z', 99) == 99
    doAssert a == {'a': 5, 'b': 9, 'z': 99}.toOrderedMap

  # XXX: does this work correctly?
  if key notin t:
    t[key] = val
  return t[key]

proc len*[A, B](t: OrderedMap[A, B]): int {.inline.} =
  ## Returns the number of keys in ``t``.
  runnableExamples:
    let a = {'a': 5, 'b': 9}.toOrderedMap
    doAssert len(a) == 2

  result = t.data.len

proc add*[A, B](t: var OrderedMap[A, B], key: A, val: B) =
  raise newException(Exception, "this function is not available when using BTree-based Maps")

proc del*[A, B](t: var OrderedMap[A, B], key: A) =
  ## Deletes ``key`` from map ``t``.
  ## Does nothing if the key does not exist.
  ##
  ## **NOTE**: This proc is destructive: the original order of the elements
  ## is not preserved!
  ##
  ## If you want to keep the order of elements after removal,
  ## use `delete proc<#delete,OrderedMap[A,B],A>`_.
  ##
  ## See also:
  ## * `delete proc<#delete,OrderedMap[A,B],A>`_
  ## * `pop proc<#pop,OrderedMap[A,B],A,B>`_
  ## * `clear proc<#clear,OrderedMap[A,B]>`_ to empty the whole map
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.toOrderedMap
    a.del('a')
    doAssert a == {'c': 13, 'b': 9}.toOrderedMap
    a.del('z')
    doAssert a == {'c': 13, 'b': 9}.toOrderedMap

  var keyIndex = -1
  if isSmall(t):
    for i, (k, _) in t.data:
      if k == key:
        keyIndex = i
    if keyIndex >= 0:
      t.data.del(keyIndex)
  else:
    keyIndex = t.byKey.getOrDefault(key, -1)
    if keyIndex >= 0:
      t.data.del(keyIndex)
      t.byKey.del(key)
      if keyIndex < t.data.len: # it wasn't the last element
        var newKey = t.data[keyIndex][0]
        t.byKey[newKey] = keyIndex

proc delete*[A, B](t: var OrderedMap[A, B], key: A) =
  ## Deletes ``key`` from map ``t``. Does nothing if the key does not exist.
  ##
  ## O(n) complexity.
  ##
  ## See also:
  ## * `del proc<#del,OrderedMap[A,B],A>`_ for faster version which doesn't
  ##   preserve the order
  ## * `pop proc<#pop,OrderedMap[A,B],A,B>`_
  ## * `clear proc<#clear,OrderedMap[A,B]>`_ to empty the whole map
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.toOrderedMap
    a.delete('a')
    doAssert a == {'b': 9, 'c': 13}.toOrderedMap
    a.delete('z')
    doAssert a == {'b': 9, 'c': 13}.toOrderedMap

  var keyIndex = -1
  if isSmall(t):
    for i, (k, _) in t.data:
      if k == key:
        keyIndex = i
    if keyIndex >= 0:
      t.data.delete(keyIndex)
  else:
    keyIndex = t.byKey.getOrDefault(key, -1)
    if keyIndex >= 0:
      t.data.delete(keyIndex)
      t.byKey = initMap[A, int]()
      populateByKey(t)

proc pop*[A, B](t: var OrderedMap[A, B], key: A, val: var B): bool =
  ## Deletes the ``key`` from the map.
  ## Returns ``true``, if the ``key`` existed, and sets ``val`` to the
  ## mapping of the key. Otherwise, returns ``false``, and the ``val`` is
  ## unchanged.
  ##
  ## O(n) complexity.
  ##
  ## See also:
  ## * `del proc<#del,OrderedMap[A,B],A>`_
  ## * `delete proc<#delete,OrderedMap[A,B],A>`_
  ## * `clear proc<#clear,OrderedMap[A,B]>`_ to empty the whole map
  runnableExamples:
    var
      a = {'c': 5, 'b': 9, 'a': 13}.toOrderedMap
      i: int
    doAssert a.pop('b', i) == true
    doAssert a == {'c': 5, 'a': 13}.toOrderedMap
    doAssert i == 9
    i = 0
    doAssert a.pop('z', i) == false
    doAssert a == {'c': 5, 'a': 13}.toOrderedMap
    doAssert i == 0

  result = key in t
  if result:
    val = t[key]
    t.del(key)

proc clear*[A, B](t: var OrderedMap[A, B]) =
  ## Resets the map so that it is empty.
  ##
  ## See also:
  ## * `del proc<#del,OrderedMap[A,B],A>`_
  ## * `delete proc<#delete,OrderedMap[A,B],A>`_
  ## * `pop proc<#pop,OrderedMap[A,B],A,B>`_
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.toOrderedMap
    doAssert len(a) == 3
    clear(a)
    doAssert len(a) == 0

  t.data.setLen(0)
  t.byKey = initMap[A, int]()


template dollarImpl(): untyped {.dirty.} =
  if t.data.len == 0:
    result = "{:}"
  else:
    result = "{"
    for (k, v) in t.data:
      if result.len > 1: result.add(", ")
      result.addQuoted(k)
      result.add(": ")
      result.addQuoted(v)
    result.add("}")

proc `$`*[A, B](t: OrderedMap[A, B]): string =
  ## The ``$`` operator for ordered maps. Used internally when calling
  ## `echo` on a map.
  dollarImpl()

proc `==`*[A, B](s, t: OrderedMap[A, B]): bool =
  ## The ``==`` operator for ordered maps. Returns ``true`` if both the
  ## content and the order are equal.
  runnableExamples:
    let
      a = {'a': 5, 'b': 9, 'c': 13}.toOrderedMap
      b = {'b': 9, 'c': 13, 'a': 5}.toOrderedMap
    doAssert a != b

  if s.data.len != t.data.len:
    return false
  for i in 0 ..< s.data.len:
    if s.data[i] != t.data[i]:
      return false
  return true



iterator pairs*[A, B](t: OrderedMap[A, B]): (A, B) =
  ## Iterates over any ``(key, value)`` pair in the map ``t`` in insertion
  ## order.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,OrderedMap[A,B]>`_
  ## * `keys iterator<#keys.i,OrderedMap[A,B]>`_
  ## * `values iterator<#values.i,OrderedMap[A,B]>`_
  ##
  ## **Examples:**
  ##
  ## .. code-block::
  ##   let a = {
  ##     'o': [1, 5, 7, 9],
  ##     'e': [2, 4, 6, 8]
  ##     }.toOrderedMap
  ##
  ##   for k, v in a.pairs:
  ##     echo "key: ", k
  ##     echo "value: ", v
  ##
  ##   # key: o
  ##   # value: [1, 5, 7, 9]
  ##   # key: e
  ##   # value: [2, 4, 6, 8]
  for p in t.data:
    yield p

iterator mpairs*[A, B](t: var OrderedMap[A, B]): (A, var B) =
  ## Iterates over any ``(key, value)`` pair in the map ``t`` (must be
  ## declared as `var`) in insertion order. The values can be modified.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,OrderedMap[A,B]>`_
  ## * `mvalues iterator<#mvalues.i,OrderedMap[A,B]>`_
  runnableExamples:
    var a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.toOrderedMap
    for k, v in a.mpairs:
      v.add(v[0] + 10)
    doAssert a == {'o': @[1, 5, 7, 9, 11],
                   'e': @[2, 4, 6, 8, 12]}.toOrderedMap

  for i in 0 ..< t.data.len:
    yield (t.data[i][0], t.data[i][1])

iterator keys*[A, B](t: OrderedMap[A, B]): A =
  ## Iterates over any key in the map ``t`` in insertion order.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,OrderedMap[A,B]>`_
  ## * `values iterator<#values.i,OrderedMap[A,B]>`_
  runnableExamples:
    var a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.toOrderedMap
    for k in a.keys:
      a[k].add(99)
    doAssert a == {'o': @[1, 5, 7, 9, 99],
                   'e': @[2, 4, 6, 8, 99]}.toOrderedMap

  for (k, _) in t.data:
    yield k

iterator values*[A, B](t: OrderedMap[A, B]): B =
  ## Iterates over any value in the map ``t`` in insertion order.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,OrderedMap[A,B]>`_
  ## * `keys iterator<#keys.i,OrderedMap[A,B]>`_
  ## * `mvalues iterator<#mvalues.i,OrderedMap[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.toOrderedMap
    for v in a.values:
      doAssert v.len == 4

  for (_, v) in t.data:
    yield v

iterator mvalues*[A, B](t: var OrderedMap[A, B]): var B =
  ## Iterates over any value in the map ``t`` (must be
  ## declared as `var`) in insertion order. The values
  ## can be modified.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,OrderedMap[A,B]>`_
  ## * `values iterator<#values.i,OrderedMap[A,B]>`_
  runnableExamples:
    var a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.toOrderedMap
    for v in a.mvalues:
      v.add(99)
    doAssert a == {'o': @[1, 5, 7, 9, 99],
                   'e': @[2, 4, 6, 8, 99]}.toOrderedMap

  for i in 0 ..< t.data.len:
    yield t.data[i][1]







# ---------------------------------------------------------------------------
# --------------------------- OrderedMapRef -------------------------------
# ---------------------------------------------------------------------------


proc newOrderedMap*[A, B](initialSize = 64): <//>OrderedMapRef[A, B] =
  ## Creates a new ordered ref map that is empty.
  ##
  ## See also:
  ## * `newOrderedMap proc<#newOrderedMap,openArray[]>`_ for creating
  ##   an `OrderedMapRef` from a collection of `(key, value)` pairs
  ## * `initOrderedMap proc<#initOrderedMap,int>`_ for creating an
  ##   `OrderedMap`
  runnableExamples:
    let
      a = newOrderedMap[int, string]()
      b = newOrderedMap[char, seq[int]]()
  new(result)
  result[] = initOrderedMap[A, B]()

proc newOrderedMap*[A, B](pairs: openArray[(A, B)]): <//>OrderedMapRef[A, B] =
  ## Creates a new ordered ref map that contains the given ``pairs``.
  ##
  ## ``pairs`` is a container consisting of ``(key, value)`` tuples.
  ##
  ## See also:
  ## * `newOrderedMap proc<#newOrderedMap,int>`_
  ## * `toOrderedMap proc<#toOrderedMap,openArray[]>`_ for an
  ##   `OrderedMap` version
  runnableExamples:
    let a = [('a', 5), ('b', 9)]
    let b = newOrderedMap(a)
    assert b == {'a': 5, 'b': 9}.newOrderedMap

  result = newOrderedMap[A, B]()
  for key, val in items(pairs): result[key] = val


proc `[]`*[A, B](t: OrderedMapRef[A, B], key: A): var B =
  ## Retrieves the value at ``t[key]``.
  ##
  ## If ``key`` is not in ``t``, the  ``KeyError`` exception is raised.
  ## One can check with `hasKey proc<#hasKey,OrderedMapRef[A,B],A>`_ whether
  ## the key exists.
  ##
  ## See also:
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  ## * `[]= proc<#[]=,OrderedMapRef[A,B],A,B>`_ for inserting a new
  ##   (key, value) pair in the map
  ## * `hasKey proc<#hasKey,OrderedMapRef[A,B],A>`_ for checking if
  ##   a key is in the map
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newOrderedMap
    doAssert a['a'] == 5
    doAssertRaises(KeyError):
      echo a['z']
  result = t[][key]

proc `[]=`*[A, B](t: OrderedMapRef[A, B], key: A, val: B) =
  ## Inserts a ``(key, value)`` pair into ``t``.
  ##
  ## See also:
  ## * `[] proc<#[],OrderedMapRef[A,B],A>`_ for retrieving a value of a key
  ## * `hasKeyOrPut proc<#hasKeyOrPut,OrderedMapRef[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,OrderedMapRef[A,B],A,B>`_
  ## * `del proc<#del,OrderedMapRef[A,B],A>`_ for removing a key from the map
  runnableExamples:
    var a = newOrderedMap[char, int]()
    a['x'] = 7
    a['y'] = 33
    doAssert a == {'x': 7, 'y': 33}.newOrderedMap

  t[][key] = val

proc hasKey*[A, B](t: OrderedMapRef[A, B], key: A): bool =
  ## Returns true if ``key`` is in the map ``t``.
  ##
  ## See also:
  ## * `contains proc<#contains,OrderedMapRef[A,B],A>`_ for use with the `in`
  ##   operator
  ## * `[] proc<#[],OrderedMapRef[A,B],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newOrderedMap
    doAssert a.hasKey('a') == true
    doAssert a.hasKey('z') == false

  result = t[].hasKey(key)

proc contains*[A, B](t: OrderedMapRef[A, B], key: A): bool =
  ## Alias of `hasKey proc<#hasKey,OrderedMapRef[A,B],A>`_ for use with
  ## the ``in`` operator.
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newOrderedMap
    doAssert 'b' in a == true
    doAssert a.contains('z') == false

  return hasKey[A, B](t, key)

proc hasKeyOrPut*[A, B](t: var OrderedMapRef[A, B], key: A, val: B): bool =
  ## Returns true if ``key`` is in the map, otherwise inserts ``value``.
  ##
  ## See also:
  ## * `hasKey proc<#hasKey,OrderedMapRef[A,B],A>`_
  ## * `[] proc<#[],OrderedMapRef[A,B],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    var a = {'a': 5, 'b': 9}.newOrderedMap
    if a.hasKeyOrPut('a', 50):
      a['a'] = 99
    if a.hasKeyOrPut('z', 50):
      a['z'] = 99
    doAssert a == {'a': 99, 'b': 9, 'z': 50}.newOrderedMap

  result = t[].hasKeyOrPut(key, val)

proc getOrDefault*[A, B](t: OrderedMapRef[A, B], key: A): B =
  ## Retrieves the value at ``t[key]`` if ``key`` is in ``t``. Otherwise, the
  ## default initialization value for type ``B`` is returned (e.g. 0 for any
  ## integer type).
  ##
  ## See also:
  ## * `[] proc<#[],OrderedMapRef[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,OrderedMapRef[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,OrderedMapRef[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,OrderedMapRef[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newOrderedMap
    doAssert a.getOrDefault('a') == 5
    doAssert a.getOrDefault('z') == 0

  getOrDefault(t[], key)

proc getOrDefault*[A, B](t: OrderedMapRef[A, B], key: A, default: B): B =
  ## Retrieves the value at ``t[key]`` if ``key`` is in ``t``.
  ## Otherwise, ``default`` is returned.
  ##
  ## See also:
  ## * `[] proc<#[],OrderedMapRef[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,OrderedMapRef[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,OrderedMapRef[A,B],A,B>`_
  ## * `mgetOrPut proc<#mgetOrPut,OrderedMapRef[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newOrderedMap
    doAssert a.getOrDefault('a', 99) == 5
    doAssert a.getOrDefault('z', 99) == 99

  getOrDefault(t[], key, default)

proc mgetOrPut*[A, B](t: OrderedMapRef[A, B], key: A, val: B): var B =
  ## Retrieves value at ``t[key]`` or puts ``val`` if not present, either way
  ## returning a value which can be modified.
  ##
  ## See also:
  ## * `[] proc<#[],OrderedMapRef[A,B],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,OrderedMapRef[A,B],A>`_
  ## * `hasKeyOrPut proc<#hasKeyOrPut,OrderedMapRef[A,B],A,B>`_
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A>`_ to return
  ##   a default value (e.g. zero for int) if the key doesn't exist
  ## * `getOrDefault proc<#getOrDefault,OrderedMapRef[A,B],A,B>`_ to return
  ##   a custom value if the key doesn't exist
  runnableExamples:
    var a = {'a': 5, 'b': 9}.newOrderedMap
    doAssert a.mgetOrPut('a', 99) == 5
    doAssert a.mgetOrPut('z', 99) == 99
    doAssert a == {'a': 5, 'b': 9, 'z': 99}.newOrderedMap

  result = t[].mgetOrPut(key, val)


proc len*[A, B](t: OrderedMapRef[A, B]): int {.inline.} =
  ## Returns the number of keys in ``t``.
  runnableExamples:
    let a = {'a': 5, 'b': 9}.newOrderedMap
    doAssert len(a) == 2

  result = t.data.len


proc del*[A, B](t: OrderedMapRef[A, B], key: A) =
  ## Deletes ``key`` from map ``t``. Does nothing if the key does not exist.
  ##
  ## **NOTE**: This proc is destructive: the original order of the elements
  ## is not preserved!
  ##
  ## If you want to keep the order of elements after removal,
  ## use `delete proc<#delete,OrderedMapRef[A,B],A>`_.
  ##
  ## See also:
  ## * `delete proc<#delete,OrderedMapRef[A,B],A>`_
  ## * `pop proc<#pop,OrderedMapRef[A,B],A,B>`_
  ## * `clear proc<#clear,OrderedMapRef[A,B]>`_ to empty the whole map
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.newOrderedMap
    a.del('a')
    doAssert a == {'c': 13, 'b': 9}.newOrderedMap
    a.del('z')
    doAssert a == {'c': 13, 'b': 9}.newOrderedMap

  t[].del(key)

proc delete*[A, B](t: OrderedMapRef[A, B], key: A) =
  ## Deletes ``key`` from map ``t``. Does nothing if the key does not exist.
  ##
  ## O(n) complexity.
  ##
  ## See also:
  ## * `del proc<#del,OrderedMapRef[A,B],A>`_ for faster version which doesn't
  ##   preserve the order
  ## * `pop proc<#pop,OrderedMapRef[A,B],A,B>`_
  ## * `clear proc<#clear,OrderedMapRef[A,B]>`_ to empty the whole map
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.toOrderedMap
    a.delete('a')
    doAssert a == {'b': 9, 'c': 13}.toOrderedMap
    a.delete('z')
    doAssert a == {'b': 9, 'c': 13}.toOrderedMap

  t[].delete(key)

proc pop*[A, B](t: OrderedMapRef[A, B], key: A, val: var B): bool =
  ## Deletes the ``key`` from the map.
  ## Returns ``true``, if the ``key`` existed, and sets ``val`` to the
  ## mapping of the key. Otherwise, returns ``false``, and the ``val`` is
  ## unchanged.
  ##
  ## See also:
  ## * `del proc<#del,OrderedMapRef[A,B],A>`_
  ## * `clear proc<#clear,OrderedMapRef[A,B]>`_ to empty the whole map
  runnableExamples:
    var
      a = {'c': 5, 'b': 9, 'a': 13}.newOrderedMap
      i: int
    doAssert a.pop('b', i) == true
    doAssert a == {'c': 5, 'a': 13}.newOrderedMap
    doAssert i == 9
    i = 0
    doAssert a.pop('z', i) == false
    doAssert a == {'c': 5, 'a': 13}.newOrderedMap
    doAssert i == 0

  pop(t[], key, val)

proc clear*[A, B](t: OrderedMapRef[A, B]) =
  ## Resets the map so that it is empty.
  ##
  ## See also:
  ## * `del proc<#del,OrderedMapRef[A,B],A>`_
  runnableExamples:
    var a = {'a': 5, 'b': 9, 'c': 13}.newOrderedMap
    doAssert len(a) == 3
    clear(a)
    doAssert len(a) == 0

  clear(t[])

proc `$`*[A, B](t: OrderedMapRef[A, B]): string =
  ## The ``$`` operator for ordered maps. Used internally when calling
  ## `echo` on a map.
  dollarImpl()

proc `==`*[A, B](s, t: OrderedMapRef[A, B]): bool =
  ## The ``==`` operator for ordered maps. Returns true if either both
  ## maps are ``nil``, or neither is ``nil`` and the content and the order of
  ## both are equal.
  runnableExamples:
    let
      a = {'a': 5, 'b': 9, 'c': 13}.newOrderedMap
      b = {'b': 9, 'c': 13, 'a': 5}.newOrderedMap
    doAssert a != b

  if isNil(s): result = isNil(t)
  elif isNil(t): result = false
  else: result = s[] == t[]



iterator keys*[A, B](t: OrderedMapRef[A, B]): A =
  ## Iterates over any key in the map ``t`` in insertion order.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,OrderedMapRef[A,B]>`_
  ## * `values iterator<#values.i,OrderedMapRef[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.newOrderedMap
    for k in a.keys:
      a[k].add(99)
    doAssert a == {'o': @[1, 5, 7, 9, 99], 'e': @[2, 4, 6, 8,
        99]}.newOrderedMap

  for k in keys(t[]):
    yield k

iterator values*[A, B](t: OrderedMapRef[A, B]): B =
  ## Iterates over any value in the map ``t`` in insertion order.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,OrderedMapRef[A,B]>`_
  ## * `keys iterator<#keys.i,OrderedMapRef[A,B]>`_
  ## * `mvalues iterator<#mvalues.i,OrderedMapRef[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.newOrderedMap
    for v in a.values:
      doAssert v.len == 4

  for v in values(t[]):
    yield v

iterator mvalues*[A, B](t: OrderedMapRef[A, B]): var B =
  ## Iterates over any value in the map ``t`` in insertion order. The values
  ## can be modified.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,OrderedMapRef[A,B]>`_
  ## * `values iterator<#values.i,OrderedMapRef[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.newOrderedMap
    for v in a.mvalues:
      v.add(99)
    doAssert a == {'o': @[1, 5, 7, 9, 99],
                   'e': @[2, 4, 6, 8, 99]}.newOrderedMap

  for v in mvalues(t[]):
    yield v

iterator pairs*[A, B](t: OrderedMapRef[A, B]): (A, B) =
  ## Iterates over any ``(key, value)`` pair in the map ``t`` in insertion
  ## order.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,OrderedMapRef[A,B]>`_
  ## * `keys iterator<#keys.i,OrderedMapRef[A,B]>`_
  ## * `values iterator<#values.i,OrderedMapRef[A,B]>`_
  ##
  ## **Examples:**
  ##
  ## .. code-block::
  ##   let a = {
  ##     'o': [1, 5, 7, 9],
  ##     'e': [2, 4, 6, 8]
  ##     }.newOrderedMap
  ##
  ##   for k, v in a.pairs:
  ##     echo "key: ", k
  ##     echo "value: ", v
  ##
  ##   # key: o
  ##   # value: [1, 5, 7, 9]
  ##   # key: e
  ##   # value: [2, 4, 6, 8]

  for p in pairs(t[]):
    yield p

iterator mpairs*[A, B](t: OrderedMapRef[A, B]): (A, var B) =
  ## Iterates over any ``(key, value)`` pair in the map ``t`` in insertion
  ## order. The values can be modified.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,OrderedMapRef[A,B]>`_
  ## * `mvalues iterator<#mvalues.i,OrderedMapRef[A,B]>`_
  runnableExamples:
    let a = {
      'o': @[1, 5, 7, 9],
      'e': @[2, 4, 6, 8]
      }.newOrderedMap
    for k, v in a.mpairs:
      v.add(v[0] + 10)
    doAssert a == {'o': @[1, 5, 7, 9, 11],
                   'e': @[2, 4, 6, 8, 12]}.newOrderedMap

  for (k, v) in mpairs(t[]):
    yield (k, v)





# -------------------------------------------------------------------------
# ------------------------------ CountMap -------------------------------
# -------------------------------------------------------------------------

type
  CountMap*[A] = object
    ## Map that counts the number of each key.
    ##
    ## For creating an empty CountMap, use `initCountMap proc
    ## <#initCountMap,int>`_.
    data: Map[A, int]

  CountMapRef*[A] = ref CountMap[A] ## Ref version of
    ## `CountMap<#CountMap>`_.
    ##
    ## For creating a new empty CountMapRef, use `newCountMap proc
    ## <#newCountMap,int>`_.

proc inc*[A](t: var CountMap[A], key: A, val: Positive = 1)


proc initCountMap*[A](initialSize = 64): CountMap[A] =
  ## Creates a new count map that is empty.
  ##
  ## Starting from Nim v0.20, maps are initialized by default and it is
  ## not necessary to call this function explicitly.
  ##
  ## See also:
  ## * `toCountMap proc<#toCountMap,openArray[A]>`_
  ## * `newCountMap proc<#newCountMap,int>`_ for creating a
  ##   `CountMapRef`
  result = CountMap[A](data: initMap[A, int]())

proc toCountMap*[A](keys: openArray[A]): CountMap[A] =
  ## Creates a new count map with every member of a container ``keys``
  ## having a count of how many times it occurs in that container.
  result = initCountMap[A](keys.len)
  for key in items(keys): result.inc(key)

proc `[]`*[A](t: CountMap[A], key: A): int =
  ## Retrieves the value at ``t[key]`` if ``key`` is in ``t``.
  ## Otherwise ``0`` is returned.
  ##
  ## See also:
  ## * `getOrDefault<#getOrDefault,CountMap[A],A,int>`_ to return
  ##   a custom value if the key doesn't exist
  ## * `mget proc<#mget,CountMap[A],A>`_
  ## * `[]= proc<#[]%3D,CountMap[A],A,int>`_ for inserting a new
  ##   (key, value) pair in the map
  ## * `hasKey proc<#hasKey,CountMap[A],A>`_ for checking if a key
  ##   is in the map
  getOrDefault(t.data, key)

proc `[]=`*[A](t: var CountMap[A], key: A, val: int) =
  ## Inserts a ``(key, value)`` pair into ``t``.
  ##
  ## See also:
  ## * `[] proc<#[],CountMap[A],A>`_ for retrieving a value of a key
  ## * `inc proc<#inc,CountMap[A],A,Positive>`_ for incrementing a
  ##   value of a key
  if val == 0:
    t.data.del(key)
  else:
    t.data[key] = val

proc inc*[A](t: var CountMap[A], key: A, val: Positive = 1) =
  ## Increments ``t[key]`` by ``val`` (default: 1).
  ##
  ## ``val`` must be a positive number. If you need to decrement a value,
  ## use a regular ``Map`` instead.
  runnableExamples:
    var a = toCountMap("aab")
    a.inc('a')
    a.inc('b', 10)
    doAssert a == toCountMap("aaabbbbbbbbbbb")

  var newValue = t[key] + val
  if newValue == 0:
    t.data.del(key)
  else:
    t.data[key] = newValue


proc smallest*[A](t: CountMap[A]): tuple[key: A, val: int] =
  ## Returns the ``(key, value)`` pair with the smallest ``val``. Efficiency: O(n)
  ##
  ## See also:
  ## * `largest proc<#largest,CountMap[A]>`_
  var first = true
  for (k, v) in pairs(t.data):
    if first or v < result.val:
      result.key = k
      result.val = v
      first = false

proc largest*[A](t: CountMap[A]): tuple[key: A, val: int] =
  ## Returns the ``(key, value)`` pair with the largest ``val``. Efficiency: O(n)
  ##
  ## See also:
  ## * `smallest proc<#smallest,CountMap[A]>`_
  var first = true
  for (k, v) in pairs(t.data):
    if first or v > result.val:
      result.key = k
      result.val = v
      first = false

proc hasKey*[A](t: CountMap[A], key: A): bool =
  ## Returns true if ``key`` is in the map ``t``.
  ##
  ## See also:
  ## * `contains proc<#contains,CountMap[A],A>`_ for use with the `in`
  ##   operator
  ## * `[] proc<#[],CountMap[A],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,CountMap[A],A,int>`_ to return
  ##   a custom value if the key doesn't exist
  hasKey(t.data, key)

proc contains*[A](t: CountMap[A], key: A): bool =
  ## Alias of `hasKey proc<#hasKey,CountMap[A],A>`_ for use with
  ## the ``in`` operator.
  return hasKey[A](t, key)

proc getOrDefault*[A](t: CountMap[A], key: A; default: int = 0): int =
  ## Retrieves the value at ``t[key]`` if``key`` is in ``t``. Otherwise, the
  ## integer value of ``default`` is returned.
  ##
  ## See also:
  ## * `[] proc<#[],CountMap[A],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,CountMap[A],A>`_ for checking if a key
  ##   is in the map
  getOrDefault(t.data, key, default)

proc len*[A](t: CountMap[A]): int =
  ## Returns the number of keys in ``t``.
  t.data.len

proc del*[A](t: var CountMap[A], key: A) =
  ## Deletes ``key`` from map ``t``. Does nothing if the key does not exist.
  ##
  ## O(n) complexity.
  ##
  ## See also:
  ## * `pop proc<#pop,CountMap[A],A,int>`_
  ## * `clear proc<#clear,CountMap[A]>`_ to empty the whole map
  runnableExamples:
    var a = toCountMap("aabbbccccc")
    a.del('b')
    assert a == toCountMap("aaccccc")
    a.del('b')
    assert a == toCountMap("aaccccc")
    a.del('c')
    assert a == toCountMap("aa")

  del(t.data, key)

proc pop*[A](t: var CountMap[A], key: A, val: var int): bool =
  ## Deletes the ``key`` from the map.
  ## Returns ``true``, if the ``key`` existed, and sets ``val`` to the
  ## mapping of the key. Otherwise, returns ``false``, and the ``val`` is
  ## unchanged.
  ##
  ## O(n) complexity.
  ##
  ## See also:
  ## * `del proc<#del,CountMap[A],A>`_
  ## * `clear proc<#clear,CountMap[A]>`_ to empty the whole map
  runnableExamples:
    var a = toCountMap("aabbbccccc")
    var i = 0
    assert a.pop('b', i)
    assert i == 3
    i = 99
    assert not a.pop('b', i)
    assert i == 99

  pop(t.data, key, val)

proc clear*[A](t: var CountMap[A]) =
  ## Resets the map so that it is empty.
  ##
  ## See also:
  ## * `del proc<#del,CountMap[A],A>`_
  ## * `pop proc<#pop,CountMap[A],A,int>`_

  # XXX: can we simplify it like this?
  t.data = initMap[A, int]()

proc merge*[A](s: var CountMap[A], t: CountMap[A]) =
  ## Merges the second map into the first one (must be declared as `var`).
  runnableExamples:
    var a = toCountMap("aaabbc")
    let b = toCountMap("bcc")
    a.merge(b)
    doAssert a == toCountMap("aaabbbccc")

  for (k, v) in pairs(t.data):
    s.inc(k, v)

proc `$`*[A](t: CountMap[A]): string =
  ## The ``$`` operator for count maps. Used internally when calling `echo`
  ## on a map.
  `$`(t.data)

proc `==`*[A](s, t: CountMap[A]): bool =
  ## The ``==`` operator for count maps. Returns ``true`` if both maps
  ## contain the same keys with the same count. Insert order does not matter.
  if s.data.len != t.data.len:
    return false
  for (k, v) in pairs(s.data):
    if t.data[k] != v:
      return false
  return true

iterator pairs*[A](t: CountMap[A]): (A, int) =
  ## Iterates over any ``(key, value)`` pair in the map ``t``.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,CountMap[A]>`_
  ## * `keys iterator<#keys.i,CountMap[A]>`_
  ## * `values iterator<#values.i,CountMap[A]>`_
  ##
  ## **Examples:**
  ##
  ## .. code-block::
  ##   let a = toCountMap("abracadabra")
  ##
  ##   for k, v in pairs(a):
  ##     echo "key: ", k
  ##     echo "value: ", v
  ##
  ##   # key: a
  ##   # value: 5
  ##   # key: b
  ##   # value: 2
  ##   # key: c
  ##   # value: 1
  ##   # key: d
  ##   # value: 1
  ##   # key: r
  ##   # value: 2
  for p in pairs(t.data):
    yield p

iterator mpairs*[A](t: var CountMap[A]): (A, var int) =
  ## Iterates over any ``(key, value)`` pair in the map ``t`` (must be
  ## declared as `var`). The values can be modified.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,CountMap[A]>`_
  ## * `mvalues iterator<#mvalues.i,CountMap[A]>`_
  runnableExamples:
    var a = toCountMap("abracadabra")
    for k, v in mpairs(a):
      v = 2
    doAssert a == toCountMap("aabbccddrr")

  for (k, v) in mpairs(t.data):
    yield (k, v)

iterator keys*[A](t: CountMap[A]): A =
  ## Iterates over any key in the map ``t``.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,CountMap[A]>`_
  ## * `values iterator<#values.i,CountMap[A]>`_
  runnableExamples:
    var a = toCountMap("abracadabra")
    for k in keys(a):
      a[k] = 2
    doAssert a == toCountMap("aabbccddrr")

  for k in keys(t.data):
    yield k

iterator values*[A](t: CountMap[A]): int =
  ## Iterates over any value in the map ``t``.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,CountMap[A]>`_
  ## * `keys iterator<#keys.i,CountMap[A]>`_
  ## * `mvalues iterator<#mvalues.i,CountMap[A]>`_
  runnableExamples:
    let a = toCountMap("abracadabra")
    for v in values(a):
      assert v < 10

  for v in values(t.data):
    yield v

iterator mvalues*[A](t: var CountMap[A]): var int =
  ## Iterates over any value in the map ``t`` (must be
  ## declared as `var`). The values can be modified.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,CountMap[A]>`_
  ## * `values iterator<#values.i,CountMap[A]>`_
  runnableExamples:
    var a = toCountMap("abracadabra")
    for v in mvalues(a):
      v = 2
    doAssert a == toCountMap("aabbccddrr")

  for v in mvalues(t.data):
    yield v





# ---------------------------------------------------------------------------
# ---------------------------- CountMapRef --------------------------------
# ---------------------------------------------------------------------------


proc inc*[A](t: CountMapRef[A], key: A, val = 1)

proc newCountMap*[A](initialSize = 64): <//>CountMapRef[A] =
  ## Creates a new ref count map that is empty.
  ##
  ## See also:
  ## * `newCountMap proc<#newCountMap,openArray[A]>`_ for creating
  ##   a `CountMapRef` from a collection
  ## * `initCountMap proc<#initCountMap,int>`_ for creating a
  ##   `CountMap`
  new(result)
  result[] = initCountMap[A]()

proc newCountMap*[A](keys: openArray[A]): <//>CountMapRef[A] =
  ## Creates a new ref count map with every member of a container ``keys``
  ## having a count of how many times it occurs in that container.
  result = newCountMap[A]()
  for key in items(keys): result.inc(key)

proc `[]`*[A](t: CountMapRef[A], key: A): int =
  ## Retrieves the value at ``t[key]`` if ``key`` is in ``t``.
  ## Otherwise ``0`` is returned.
  ##
  ## See also:
  ## * `getOrDefault<#getOrDefault,CountMapRef[A],A,int>`_ to return
  ##   a custom value if the key doesn't exist
  ## * `mget proc<#mget,CountMapRef[A],A>`_
  ## * `[]= proc<#[]%3D,CountMapRef[A],A,int>`_ for inserting a new
  ##   (key, value) pair in the map
  ## * `hasKey proc<#hasKey,CountMapRef[A],A>`_ for checking if a key
  ##   is in the map
  result = t[][key]


proc `[]=`*[A](t: CountMapRef[A], key: A, val: int) =
  ## Inserts a ``(key, value)`` pair into ``t``.
  ##
  ## See also:
  ## * `[] proc<#[],CountMapRef[A],A>`_ for retrieving a value of a key
  ## * `inc proc<#inc,CountMapRef[A],A,int>`_ for incrementing a
  ##   value of a key
  assert val > 0
  t[][key] = val

proc inc*[A](t: CountMapRef[A], key: A, val = 1) =
  ## Increments ``t[key]`` by ``val`` (default: 1).
  runnableExamples:
    var a = newCountMap("aab")
    a.inc('a')
    a.inc('b', 10)
    doAssert a == newCountMap("aaabbbbbbbbbbb")
  t[].inc(key, val)

proc smallest*[A](t: CountMapRef[A]): (A, int) =
  ## Returns the ``(key, value)`` pair with the smallest ``val``. Efficiency: O(n)
  ##
  ## See also:
  ## * `largest proc<#largest,CountMapRef[A]>`_
  t[].smallest

proc largest*[A](t: CountMapRef[A]): (A, int) =
  ## Returns the ``(key, value)`` pair with the largest ``val``. Efficiency: O(n)
  ##
  ## See also:
  ## * `smallest proc<#smallest,CountMap[A]>`_
  t[].largest

proc hasKey*[A](t: CountMapRef[A], key: A): bool =
  ## Returns true if ``key`` is in the map ``t``.
  ##
  ## See also:
  ## * `contains proc<#contains,CountMapRef[A],A>`_ for use with the `in`
  ##   operator
  ## * `[] proc<#[],CountMapRef[A],A>`_ for retrieving a value of a key
  ## * `getOrDefault proc<#getOrDefault,CountMapRef[A],A,int>`_ to return
  ##   a custom value if the key doesn't exist
  result = t[].hasKey(key)

proc contains*[A](t: CountMapRef[A], key: A): bool =
  ## Alias of `hasKey proc<#hasKey,CountMapRef[A],A>`_ for use with
  ## the ``in`` operator.
  return hasKey[A](t, key)

proc getOrDefault*[A](t: CountMapRef[A], key: A, default: int): int =
  ## Retrieves the value at ``t[key]`` if``key`` is in ``t``. Otherwise, the
  ## integer value of ``default`` is returned.
  ##
  ## See also:
  ## * `[] proc<#[],CountMapRef[A],A>`_ for retrieving a value of a key
  ## * `hasKey proc<#hasKey,CountMapRef[A],A>`_ for checking if a key
  ##   is in the map
  result = t[].getOrDefault(key, default)

proc len*[A](t: CountMapRef[A]): int =
  ## Returns the number of keys in ``t``.
  result = t.data.len

proc del*[A](t: CountMapRef[A], key: A) =
  ## Deletes ``key`` from map ``t``. Does nothing if the key does not exist.
  ##
  ## O(n) complexity.
  ##
  ## See also:
  ## * `pop proc<#pop,CountMapRef[A],A,int>`_
  ## * `clear proc<#clear,CountMapRef[A]>`_ to empty the whole map
  del(t[], key)

proc pop*[A](t: CountMapRef[A], key: A, val: var int): bool =
  ## Deletes the ``key`` from the map.
  ## Returns ``true``, if the ``key`` existed, and sets ``val`` to the
  ## mapping of the key. Otherwise, returns ``false``, and the ``val`` is
  ## unchanged.
  ##
  ## O(n) complexity.
  ##
  ## See also:
  ## * `del proc<#del,CountMapRef[A],A>`_
  ## * `clear proc<#clear,CountMapRef[A]>`_ to empty the whole map
  pop(t[], key, val)

proc clear*[A](t: CountMapRef[A]) =
  ## Resets the map so that it is empty.
  ##
  ## See also:
  ## * `del proc<#del,CountMapRef[A],A>`_
  ## * `pop proc<#pop,CountMapRef[A],A,int>`_
  clear(t[])

proc merge*[A](s, t: CountMapRef[A]) =
  ## Merges the second map into the first one.
  runnableExamples:
    let
      a = newCountMap("aaabbc")
      b = newCountMap("bcc")
    a.merge(b)
    doAssert a == newCountMap("aaabbbccc")

  s[].merge(t[])


proc `$`*[A](t: CountMapRef[A]): string =
  ## The ``$`` operator for count maps. Used internally when calling `echo`
  ## on a map.
  `$`(t.data)

proc `==`*[A](s, t: CountMapRef[A]): bool =
  ## The ``==`` operator for count maps. Returns ``true`` if either both maps
  ## are ``nil``, or neither is ``nil`` and both contain the same keys with the same
  ## count. Insert order does not matter.
  if isNil(s): result = isNil(t)
  elif isNil(t): result = false
  else: result = s[] == t[]




iterator keys*[A](t: CountMapRef[A]): A =
  ## Iterates over any key in the map ``t``.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,CountMap[A]>`_
  ## * `values iterator<#values.i,CountMap[A]>`_
  runnableExamples:
    let a = newCountMap("abracadabra")
    for k in keys(a):
      a[k] = 2
    doAssert a == newCountMap("aabbccddrr")

  for k in keys(t[]):
    yield k

iterator values*[A](t: CountMapRef[A]): int =
  ## Iterates over any value in the map ``t``.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,CountMapRef[A]>`_
  ## * `keys iterator<#keys.i,CountMapRef[A]>`_
  ## * `mvalues iterator<#mvalues.i,CountMapRef[A]>`_
  runnableExamples:
    let a = newCountMap("abracadabra")
    for v in values(a):
      assert v < 10

  for v in values(t[]):
    yield v

iterator mvalues*[A](t: CountMapRef[A]): var int =
  ## Iterates over any value in the map ``t``. The values can be modified.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,CountMapRef[A]>`_
  ## * `values iterator<#values.i,CountMapRef[A]>`_
  runnableExamples:
    var a = newCountMap("abracadabra")
    for v in mvalues(a):
      v = 2
    doAssert a == newCountMap("aabbccddrr")

  for v in mvalues(t[]):
    yield v

iterator pairs*[A](t: CountMapRef[A]): (A, int) =
  ## Iterates over any ``(key, value)`` pair in the map ``t``.
  ##
  ## See also:
  ## * `mpairs iterator<#mpairs.i,CountMapRef[A]>`_
  ## * `keys iterator<#keys.i,CountMapRef[A]>`_
  ## * `values iterator<#values.i,CountMapRef[A]>`_
  ##
  ## **Examples:**
  ##
  ## .. code-block::
  ##   let a = newCountMap("abracadabra")
  ##
  ##   for k, v in pairs(a):
  ##     echo "key: ", k
  ##     echo "value: ", v
  ##
  ##   # key: a
  ##   # value: 5
  ##   # key: b
  ##   # value: 2
  ##   # key: c
  ##   # value: 1
  ##   # key: d
  ##   # value: 1
  ##   # key: r
  ##   # value: 2
  for p in pairs(t[]):
    yield p

iterator mpairs*[A](t: CountMapRef[A]): (A, var int) =
  ## Iterates over any ``(key, value)`` pair in the map ``t``. The values can
  ## be modified.
  ##
  ## See also:
  ## * `pairs iterator<#pairs.i,CountMapRef[A]>`_
  ## * `mvalues iterator<#mvalues.i,CountMapRef[A]>`_
  runnableExamples:
    let a = newCountMap("abracadabra")
    for k, v in mpairs(a):
      v = 2
    doAssert a == newCountMap("aabbccddrr")

  for (k, v) in mpairs(t[]):
    yield (k, v)
