import unittest
import fusion/map


test "init":
  var a = initCountMap[char]()
  var b: CountMap[char]
  doAssert a == b
  a['b'] = 3
  b['b'] = 2
  doAssert a != b
  b.inc('b')
  doAssert a == b

test "getters":
  var a = toCountMap("abracadabra")
  doAssert a.len == 5
  doAssert a['a'] == 5
  doAssert a['b'] == 2
  doAssert 'c' in a
  doAssert 'z' notin a
  doAssert a['z'] == 0
  doAssert a.getOrDefault('a', 99) == 5
  doAssert a.getOrDefault('z', 99) == 99
  doAssert 'z' notin a

test "setters":
  var a = toCountMap("aab")
  doAssert a.len == 2
  a['a'] = 0
  doAssert a.len == 1
  a['a'] = -1
  doAssert a.len == 2
  a.inc('a')
  doAssert a.len == 1

test "largest and smallest":
  var a = toCountMap("abracadabra")
  doAssert a.largest() == ('a', 5)
  doAssert a.smallest() == ('c', 1)
  var b = toCountMap("millimeter")
  doAssert b.largest() == ('e', 2)
  doAssert b.smallest() == ('r', 1)

test "equality":
  var a = toCountMap("aab")
  a.inc('a')
  a.inc('b', 10)
  doAssert a == toCountMap("aaabbbbbbbbbbb")
  doAssert a == toCountMap("bbbbbbbbbbbaaa")
  doAssert a == toCountMap("abbbbbbbbbbbaa")
  doAssert a != toCountMap("baa")

test "remove":
  var a = toCountMap("abracadabra")
  doAssert a.len == 5
  a.del('a')
  doAssert a.len == 4
  a.del('a')
  doAssert a.len == 4
  a.del('z')
  doAssert a.len == 4
  a['a'] = 13
  doAssert a.len == 5
  a.del('a')
  doAssert a.len == 4
  var i = 1234
  doAssert not a.pop('a', i)
  doAssert i == 1234
  doAssert a.pop('b', i)
  doAssert i == 2
  doAssert a.len == 3
  a.clear()
  doAssert a.len == 0
  doAssert $a == "{:}"
  a.del('z')
  a.clear()

test "merge":
  var a = toCountMap("aaabbc")
  var b = toCountMap("aab")
  merge(a, b)
  doAssert a.len == 3
  doAssert a['b'] == 3

test "mvalues and mpairs":
  var a = toCountMap("aaabbc");
  for k, v in mpairs(a):
    v += 10
  doAssert a['a'] == 13
  doAssert a['b'] == 12
  doAssert a['c'] == 11
  for v in mvalues(a):
    v += 10
  doAssert a['a'] == 23
  doAssert a['b'] == 22
  doAssert a['c'] == 21
