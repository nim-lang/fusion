import std/[strutils, sequtils, strformat,
            macros, options, tables, json]

import fusion/matching
{.experimental: "caseStmtMacros".}

# dumpTree:
#   mixin _

# import hmisc/hdebug_misc

#===========================  implementation  ============================#

#================================  tests  ================================#

import unittest

template assertEq(a, b: untyped): untyped =
  if a != b:
    echo a
    echo b
    raiseAssert("Comparison failed in " & $instantiationInfo())

suite "Matching":
  test "Has kind for anything":
    type
      En1 = enum
        eN11
        eN12

      Obj1 = object
        case kind: En1
         of eN11:
           f1: int
         of eN12:
           f2: float

    let val = Obj1()

  test "Pattern parser tests":
    macro main(): untyped =
      template t(body: untyped): untyped =
        block:
          parseMatchExpr: quote: body

      assert (t true).kind == kItem

      block:
        let s = t [1, 2, all @b, @a]
        assert s.seqElems[3].bindVar == some(ident("a"))
        assert s.seqElems[2].bindVar == some(ident("b"))
        assert s.seqElems[2].patt.bindVar == none(NimNode)

      discard t([1,2,3,4])
      discard t((1,2))
      discard t((@a | @b))
      discard t((a: 12, b: 2))
      # dumpTree: [(0 .. 3) @patt is JString()]
      # dumpTree: [0..3 @patt is JString()]
      discard t([(0 .. 3) @patt is JString()])
      discard t([0..3 @patt is JString()])

      block:
        let node = quote: (12 .. 33)

        (
          Par([Infix([_, @lhs, @rhs])]) |
          Command([Infix([_, @lhs, @rhs])]) |
          Infix([_, @lhs, @rhs])
        ) := node

        assert lhs == newLit(12)
        assert rhs == newLit(33)

    main()

    # block: [0 .. 2 @a is 12] := [12, 12, 12]; assert a == @[12, 12, 12]
    # block:
    #   assert not ([0 .. 2 is 12] ?= [1, 2, 3])
    #   assert not ([(0 .. 2) is 12] ?= [1, 2, 3])


  test "Simple uses":
    assertEq 12, case (12, 24):
                   of (_, 24): expr[1] div 2
                   else: raiseAssert("#[ not possible ]#")


    case [1]:
      of [_]: discard

    case [1,2,3,4]:
      of [_]: fail()
      of [_, 2, 3, _]:
        discard


    assertEq "hehe", case (true, false):
           of (true, _): "hehe"
           else: "2222"

    assert (a: 12) ?= (a: 12)
    assertEq "hello world", case (a: 12, b: 12):
           of (a: 12, b: 22): "nice"
           of (a: 12, b: _): "hello world"
           else: "default value"

    assert (a: 22, b: 90) ?= (a: 22, b: 90)
    assertEq "default fallback", case (a: 22, b: 90):
           of (b: 91): "900999"
           elif "some other" == "check": "rly?"
           elif true: "default fallback"
           else: raiseAssert("#[ not possible ! ]#")

    assertEq "000", case %{"hello" : %"world"}:
           of {"999": _}: "nice"
           of {"hello": _}: "000"
           else: "discard"

    assertEq 12, case @[12, 32]:
           of [_, 32]: expr[0]
           else: 999

    assertEq 1, case [(1, 3), (3, 4)]:
                  of [(1, _), _]: 1
                  else: 999

    assertEq 2, case (true, false):
                  of (true, true) | (false, false): 3
                  else: 2

  test "Len test":
    macro e(body: untyped): untyped =
      case body:
        of Bracket([Bracket(len: in {1 .. 3})]):
          newLit("Nested bracket !")
        of Bracket(len: in {3 .. 6}):
          newLit(expr.toStrLit().strVal() & " matched")
        else:
          newLit("not matched")

    discard e([2,3,4])
    discard e([[1, 3, 4]])
    discard e([3, 4])


  test "Regular objects":
    type
      A1 = object
        f1: int

    case A1(f1: 12):
      of (f1: 12):
        discard "> 10"
      else:
        fail()

    assertEq 10, case A1(f1: 90):
                   of (f1: 20): 80
                   else: 10

  test "Private fields":
    type
      A2 = object
        hidden: float

    func public(a: A2): string = $a.hidden

    case A2():
      of (public: _):
        discard
      else:
        fail()

    case A2(hidden: 8.0):
      of (public: "8.0"): discard
      else: fail()

  type
    En2 = enum
      enEE
      enEE1
      enZZ

    Obj2 = object
      case kind: En2
        of enEE, enEE1:
          eee: seq[Obj2]
        of enZZ:
          fl: int


  test "Case objects":
    case Obj2():
      of EE():
        discard
      of ZZ(): fail()

    case Obj2():
      of (kind: in {enEE, enZZ}): discard
      else: fail()


    when false: # FIXME
      const eKinds = {enEE, enEE1}
      case Obj2():
        of (kind: in {enEE} + eKinds): discard
        else: fail()

    case (c: (a: 12)):
      of (c: (a: _)): discard
      else: fail()

    case [(a: 12, b: 3)]:
      of [(a: 12, b: 22)]: fail()
      of [(a: _, b: _)]: discard

    case (c: [3, 3, 4]):
      of (c: [_, _, _]): discard
      of (c: _): fail()

    case (c: [(a: [1, 3])]):
      of (c: [(a: [_])]): fail()
      else: discard

    case (c: [(a: [1, 3]), (a: [1, 4])]):
      of (c: [(a: [_]), _]): fail()
      else:
        discard

    case Obj2(kind: enEE, eee: @[Obj2(kind: enZZ, fl: 12)]):
      of enEE(eee: [(kind: enZZ, fl: 12)]):
        discard
      else:
        fail()

    case Obj2():
      of enEE():
        discard
      of enZZ():
        fail()
      else:
        fail()

    case Obj2():
      of (kind: in {enEE, enEE1}):
        discard
      else:
        fail()

  test "Object items":
    func `[]`(o: Obj2, idx: int): Obj2 = o.eee[idx]
    func len(o: Obj2): int = o.eee.len

    case Obj2(kind: enEE, eee: @[Obj2(), Obj2()]):
      of [_, _]:
        discard
      else:
        fail()

    case Obj2(kind: enEE, eee: @[Obj2(), Obj2()]):
      of EE(eee: [_, _, _]): fail()
      of EE(eee: [_, _]): discard
      else: fail()

    case Obj2(kind: enEE1, eee: @[Obj2(), Obj2()]):
      of EE([_, _]):
        fail()
      of EE1([_, _, _]):
        fail()
      of EE1([_, _]):
        discard
      else:
        fail()



  test "Variable binding":
    when false: # NOTE compilation error test
      case (1, 2):
        of ($a, $a, $a, $a):
          discard
        else:
          fail()

    assertEq "122", case (a: 12, b: 2):
                      of (a: @a, b: @b): $a & $b
                      else: "✠ ♰ ♱ ☩ ☦ ☨ ☧ ⁜ ☥"

    assertEq 12, case (a: 2, b: 10):
                   of (a: @a, b: @b): a + b
                   else: 89

    assertEq 1, case (1, (3, 4, ("e", (9, 2)))):
      of (@a, _): a
      of (_, (@a, @b, _)): a + b
      of (_, (_, _, (_, (@c, @d)))): c * d
      else: 12

    proc tupleOpen(a: (bool, int)): int =
      case a:
        of (true, @capture): capture
        else: -90

    assertEq 12, tupleOpen((true, 12))

  test "Infix":
    macro a(): untyped  =
      case newPar(ident "1", ident "2"):
        of Par([@ident1, @ident2]):
          assert ident1.strVal == "1"
          assert ident2.strVal == "2"
        else:
          assert false

    a()

  test "Iflet 2":
    macro ifLet2(head: untyped,  body: untyped): untyped =
      case head[0]:
        of Asgn([@lhs is Ident(), @rhs]):
          result = quote do:
            let expr = `rhs`
            if expr.isSome():
              let `lhs` = expr.get()
              `body`
        else:
          head[0].expectKind({nnkAsgn})
          head[0][0].expectKind({nnkIdent})
          error("Expected assgn expression", head[0])

    ifLet2 (nice = some(69)):
      assert nice == 69



  test "Alternative":
    assertEq "matched", case (a: 12, c: 90):
      of (a: 12 | 90, c: _): "matched"
      else: "not matched"

    assertEq 12, case (a: 9):
                  of (a: 9 | 12): 12
                  else: 666


  test "Set":
    case {0 .. 3}:
      of {2, 3}: discard
      else: fail()

  test "Match assertions":
    [1,2,3].assertMatch([all @res]); assertEq res, @[1,2,3]
    [1,2,3].assertMatch([all @res2]); assertEq res2, @[1,2,3]
    [1,2,3].assertMatch([@first, all @other])
    assertEq first, 1
    assertEq other, @[2, 3]


    block: [@first, all @other] := [1,2,3]
    block: [_, _, _] := @[1,2,3]
    block: (@a, @b) := ("1", "2")
    block: (_, (@a, @b)) := (1, (2, 3))
    block:
      let tmp = @[1,2,3,4,5,6,5,6]
      block: [until @a == 6, .._] := tmp; assertEq a, @[1,2,3,4,5]
      block: [@a, .._] := tmp; assertEq a, 1
      block: [any @a(it < 100)] := tmp; assertEq a, tmp
      block: [pref @a is (1|2|3)] := [1,2,3]; assertEq a, @[1,2,3]
      block: [pref (1|2|3)] := [1,2,3]
      block: [until 3, _] := [1,2,3]
      block: [all 1] := [1,1,1]
      block: assert [all 1] ?= [1,1,1]
      block: assert not ([all 1] ?= [1,2,3])
      block: [opt @a or 12] := `@`[int]([]); assertEq a, 12
      block: [opt(@a or 12)] := [1]; assertEq a, 1
      block: [opt @a] := [1]; assertEq a, some(1)
      block: [opt @a] := `@`[int]([]); assertEq  a, none(int)
      block: [opt(@a)] := [1]; assertEq a, some(1)
      block:
        {"k": opt @val1 or "12"} := {"k": "22"}.toTable()
        static: assert val1 is string
        {"k": opt(@val2 or "12")} := {"k": "22"}.toTable()
        static: assert val2 is string
        assertEq val1, val2
        assertEq val1, "22"
        assertEq val2, "22"

      block:
        {"h": Some(@x)} := {"h": some("22")}.toTable()
        assert x is string
        assert x == "22"

      block:
        {"k": opt @val, "z": opt @val2} := {"z" : "2"}.toTable()
        assert val is Option[string]
        assert val.isNone()
        assert val2 is Option[string]
        assert val2.isSome()
        assert val2.get() == "2"

      block: [all(@a)] := [1]; assertEq a, @[1]
      block: (f: @hello is ("2" | "3")) := (f: "2"); assertEq hello, "2"
      block: (f: @a(it mod 2 == 0)) := (f: 2); assertEq a, 2
      block: assert not ([1,2] ?= [1,2,3])
      block: assert [1, .._] ?= [1,2,3]
      block: assert [1,2,_] ?= [1,2,3]
      block:
        ## Explicitly use `_` to match whole sequence
        [until @head is 'd', _] := "word"
        ## Can also use trailing `.._`
        [until 'd', .._] := "word"
        assertEq head, @['w', 'o', 'r']

      block:
        [
          [@a, @b],
          [@c, @d, all @e],
          [@f, @g, all @h]
        ] := @[
          @[1,2],
          @[2,3,4,5,6,7],
          @[5,6,7,2,3,4]
        ]

      block: (@a, (@b, @c), @d) := (1, (2, 3), 4)
      block: (Some(@x), @y) := (some(12), none(float))
      block: @hello != nil := (var tmp: ref int; new(tmp); tmp)

      block: [all @head] := [1,2,3]; assertEq head, @[1,2,3]
      block: [all (1|2|3|4|5)] := [1,2,3,4,1,1,2]
      block:
        [until @head is 2, all @tail] := [1,2,3]
        assertEq head, @[1]
        assertEq tail, @[2,3]

      block: (_, _, _) := (1, 2, "fa")
      block: ([1,2,3]) := [1,2,3]
      block: ({0: 1, 1: 2, 2: 3}) := {0: 1, 1: 2, 2: 3}.toTable()


    block:
      block: [0..3 is @head] := @[1,2,3,4]

    case [%*"hello", %*"12"]:
      of [any @elem is JString()]:
        discard
      else:
        fail()

    case ("foo", 78)
      of ("foo", 78):
        discard
      of ("bar", 88):
        fail()

    block: Some(@x) := some("hello")

    if (Some(@x) ?= some("hello")) and
       (Some(@y) ?= some("world")):
      assertEq x, "hello"
      assertEq y, "world"
    else:
      discard

  test "More examples":
    func butLast(a: seq[int]): int =
      case a:
        of []: raiseAssert(
          "Cannot take one but last from empty seq!")
        of [_]: raiseAssert(
          "Cannot take one but last from seq with only one element!")
        of [@pre, _]: pre
        of [_, all @tail]: butLast(tail)
        else: raiseAssert("Not possible")

    assertEq butLast(@[1,2,3,4]), 3

    func butLastGen[T](a: seq[T]): T =
      expand case a:
        of []: raiseAssert(
          "Cannot take one but last from empty seq!")
        of [_]: raiseAssert(
          "Cannot take one but last from seq with only one element!")
        of [@pre, _]: pre
        of [_, all @tail]: butLastGen(tail)
        else: raiseAssert("Not possible")

    assertEq butLastGen(@["1", "2"]), "1"

  test "Use in generics":
    func hello[T](a: seq[T]): T =
      [@head, .._] := a
      return head

    proc g1[T](a: seq[T]): T =
      case a:
        of [@a]: discard
        else: fail()

      expand case a:
        of [_]: discard
        else: fail()

      expand case a:
        of [_.startsWith("--")]: discard
        else: fail()

      expand case a:
        of [(len: < 12)]: discard
        else: fail()

    discard g1(@["---===---=="])


  test "Predicates":
    case ["hello"]:
      of [_.startsWith("--")]:
        fail()
      of [_.startsWith("==")]:
        fail()
      else:
        discard

  test "One-or-more":
    template testCase(main, patt, body: untyped): untyped =
      case main:
        of patt:
          body
        else:
          fail()
          raiseAssert("#[ IMPLEMENT ]#")

    case [1]:
      of [@a]: assertEq a, 1
      else: fail()

    case [1]:
      of [all @a]: assertEq a, @[1]
      else: fail()

    case [1,2,3,4]:
      of [_, until @a is 4, 4]:
        assertEq a, @[2,3]
      else:
        fail()


    case [1,2,3,4]:
      of [@a, .._]:
        assert a is int
        assert a == 1
      else:
        fail()


    case [1,2,3,4]:
      of [all @a]:
        assert a is seq[int]
        assert a == @[1,2,3,4]
      else:
        fail()

  test "Optional matches":
    case [1,2,3,4]:
      of [pref @a is (1 | 2), _, opt @a or 5]:
        assertEq a, @[1,2,4]


    case [1,2,3]:
      of [pref @a is (1 | 2), _, opt @a or 5]:
        assertEq a, @[1,2,5]

    case [1,2,2,1,1,1]:
      of [all (1 | @a)]:
        assert a is seq[int]
        assertEq a, @[2, 2]

  test "Tree construction":
    macro testImpl(): untyped =
      let node = makeTree(NimNode):
        IfStmt[
          ElifBranch[== ident("true"),
            Call[
              == ident("echo"),
              == newLit("12")]]]


      IfStmt[ElifBranch[@head, Call[@call, @arg]]] := node
      assertEq head, ident("true")
      assertEq call, ident("echo")
      assertEq arg, newLit("12")

      block:
        let input = "hello"
        # expandMacros:
        Ident(str: @output) := makeTree(NimNode, Ident(str: input))
        assertEq output, input


    testImpl()

  test "Tree builder custom type":
    type
      HtmlNodeKind = enum
        htmlBase = "base"
        htmlHead = "head"
        htmlLink = "link"

      HtmlNode = object
        kind*: HtmlNodeKind
        text*: string
        subn*: seq[HtmlNode]

    func add(n: var HtmlNode, s: HtmlNode) = n.subn.add s
    discard makeTree(HtmlNode, Base())
    discard makeTree(HtmlNode, base())
    discard makeTree(HtmlNode, base([link()]))
    discard makeTree(HtmlNode):
      base:
        link()

    template wrapper1(body: untyped): untyped =
      makeTree(HtmlNode):
        body

    template wrapper2(body: untyped): untyped =
      makeTree(HtmlNode, body)

    let tmp1 = wrapper1:
      base: link()
      base: link()

    assert tmp1 is seq[HtmlNode]


    let tmp3 = wrapper1:
      base:
        base: link()
        base: link()

    assert tmp3 is HtmlNode

    let tmp2 = wrapper1:
      base:
        link()

    assert tmp2 is HtmlNode

    discard wrapper2:
      base:
        link()


  test "withItCall":
    macro withItCall(head: typed, body: untyped): untyped =
      result = newStmtList()
      result.add quote do:
        var it {.inject.} = `head`

      for stmt in body:
        case stmt:
          of (
            kind: in {nnkCall, nnkCommand},
            [@head is Ident(), all @arguments]
          ):
            result.add newCall(newDotExpr(
              ident "it", head
            ), arguments)
          else:
            result.add stmt

      result.add ident("it")

      result = newBlockStmt(result)


    let res = @[12,3,3].withItCall do:
      it = it.filterIt(it < 4)
      it.add 99

  test "Examples from documentation":
    block: [@a] := [1]; assert (a is int) and (a == 1)
    block:
      {"key" : @val} := {"key" : "val"}.toTable()
      assert val is string
      assert val == "val"

    block: [any @a] := [1,2,3]; assert a is seq[int]
    block:
      [any @a(it < 3)] := [1, 2, 3]
      assert a is seq[int]
      assert a == @[1, 2]

    block:
      [until @a == 6, _] := [1, 2, 3, 6]
      assert a is seq[int]
      assert a == @[1, 2, 3]

    block:
      [all @a == 6] := [6, 6, 6]
      assert a is seq[int]
      assert a == @[6, 6, 6]

    block:
      [any @a > 100] := [1, 2, 101]
      assert @a is seq[int]
      assert @a == @[101]

    block:
      [any @a(it > 100)] := [1, 2, 101]
      [any @b > 100] := [1, 2, 101]
      assert a == b

    block:
      [_ in {2 .. 10}] := [2]

    block:
      [any @a in {2 .. 10}] := [1, 2, 3]
      [any in {2 .. 10}] := [1, 2, 3]
      [any _ in {2 .. 10}] := [1, 2, 3]

    block:
      [none @a in {6 .. 10}] := [1, 2, 3]
      assert a is seq[int]
      assert a == @[1, 2, 3]

      [none in {6 .. 10}] := [1, 2, 3]
      [none @b(it in {6 .. 10})] := [1, 2, 3]

    block:
      [opt @val or 12] := [1]
      assert val is int
      assert val  == 1

    block:
      [_, opt @val] := [1]
      assert val is Option[int]
      assert val.isNone()

    block:
      [0 .. 3 @val, _] := [1, 2, 3, 4, 5]
      assert val is seq[int]
      assert val == @[1, 2, 3, 4]
      [0 .. 1 @val1, 2 .. 3 @val2] := [1, 2, 3, 4]
      assert val1 is seq[int] and val1 == @[1, 2]
      assert val2 is seq[int] and val2 == @[3, 4]

    block:
      let val = (1, 2, "fa")
      assert (_, _, _) ?= val
      assert not ((@a, @a, _) ?= val)

    block:
      case (true, false):
        of (@a, @a):
          fail()
        of (@a, _):
          assert a == true

  test "Nested objects":
    type
      Lvl3 = object
        f3: float

      Lvl2 = object
        f2: Lvl3

      Lvl1 = object
        f1: Lvl2

    assert Lvl1().f1.f2.f3 < 10
    assert (f1.f2.f3: < 10) ?= Lvl1()

    case Lvl1():
      of (f1.f2.f3: < 10):
        discard
      of (f1: (f2: (f3: < 10))):
        discard
      else:
        fail()

  test "Nested key access":
    let val = (@[1,2,3], @[3,4,5])

    case val:
      of ((len: <= 3), (len: <= 3)):
        discard
      else:
        fail()

    let val2 = (hello: @[1,2,3])

    case val2:
      of (hello.len: <= 3):
        discard
      else:
        fail()


    let val3 = (hello3: @[@[@["eee"]]])
    assert not ((hello3[0][1][2].len: < 10) ?= val3)
    assert (hello3[0][0][0].len: < 10) ?= val3
    assert (hello3: is [[[(len: < 10)]]]) ?= val3


    let val4 = {"hello" : { "world" : "nice" }.toTable()}.toTable()
    assert { "hello"["world"] : "nice" } ?= val4


suite "Gara tests":
  ## Test suite copied from gara pattern matching
  type
    Rectangle = object
      a: int
      b: int

    Repo = ref object
      name: string
      author: Author
      commits: seq[Commit]

    Author = object
      name: string
      email: Email

    Email = object
      raw: string

    # just an example: nice for match
    CommitType = enum ctNormal, ctMerge, ctFirst, ctFix

    Commit = ref object
      message: string
      case kind: CommitType:
        of ctNormal:
          diff: string # simplified
        of ctMerge:
          original: Commit
          other: Commit
        of ctFirst:
          code: string
        of ctFix:
          fix: string

  let repo = Repo(
    name: "ExampleDB",
    author: Author(
      name: "Example Author",
      email: Email(raw: "example@exampledb.org")),
    commits: @[
      Commit(kind: ctFirst, message: "First", code: "e:0"),
      Commit(kind: ctNormal, message: "Normal", diff: "+e:2\n-e:0")
  ])

  test "Capturing":
    let a = 2
    case [a]: ## Wrap in `[]` to trigger `match` macro, otherwise it
              ## will be treated as regular case match.
      of [@b == 2]:
        assertEq b, 2
      else:
        fail()


  test "Object":
    let a = Rectangle(a: 2, b: 0)

    case a:
      of (a: 4, b: 1):
        fail()
      of (a: 2, b: @b):
        assertEq b, 0
      else :
        fail()

  test "Subpattern":
    case repo:
      of (name: "New", commits: == @[]):
        fail()
      of (
        name: @name,
        author: (
          name: "Example Author",
          email: @email
        ),
        commits: @commits
      ):
        assertEq name, "ExampleDB"
        assertEq email.raw, "example@exampledb.org"
      else:
        fail()

  test "Sequence":
    let a = @[Rectangle(a: 2, b: 4), Rectangle(a: 4, b: 4), Rectangle(a: 4, b: 4)]

    case a:
      of []:
        fail()
      of [_, all @others is (a: 4, b: 4)]:
        check(others == a[1 .. ^1])
      else:
        fail()

    # _ is always true, (a: 4, b: 4) didn't match element 2

    # _ is alway.. a.a was 4, but a.b wasn't 4 => not a match



  test "Sequence subpattern":
    let a = @[
      Rectangle(a: 2, b: 4),
      Rectangle(a: 4, b: 0),
      Rectangle(a: 4, b: 4),
      Rectangle(a: 4, b: 4)
    ]

    case a:
      of []:
        fail()
      of [_, _, all (a: @list)]:
        check(list == @[4, 4])
      else:
        fail()

  test "Variant":
    let a = Commit(kind: ctNormal, message: "e", diff: "z")

    case a:
      of Merge(original: @original, other: @other):
        fail()
      of Normal(message: @message):
        check(message == "e")
      else:
        fail()

  test "Custom unpackers":
    let email = repo.author.email

    proc data(email: Email): tuple[name: string, domain: string] =
      let words = email.raw.split('@', 1)
      (name: words[0], domain: words[1])

    proc tokens(email: Email): seq[string] =
      # work for js slow
      result = @[]
      var token = ""
      for i, c in email.raw:
        if not c.isAlphaNumeric():
          if token.len > 0:
            result.add(token)
            token = ""
          result.add($c)
        else:
          token.add(c)
      if token.len > 0:
        result.add(token)

    case email:
      of (data: (name: "academy")):
        fail()

      # WARNING FIXME multiple calls for `tokens`
      of (tokens: [_, _, _, _, @token]):
        check(token == "org")

  test "if":
    let b = @[4, 0]

    case b:
      of [_, @t(it mod 2 == 0)]:
        check(t == 0)
      else:
        fail()

  test "unification":
    let b = @["nim", "nim", "c++"]

    var res = ""
    case ["nim", "nim", "C++"]:
      of [@x, @x, @x]: discard
      of [@x, @x, _]: res = x

    assertEq res, "nim"



    case b:
      of [@x, @x, @x]:
        fail()
      of [@x, @x, _]:
        check(x == "nim")
      else:
        fail()

  test "option":
    let a = some[int](3)

    case a:
      of Some(@i):
        check(i == 3)
      else:
        fail()


  test "nameless tuple":
    let a = ("a", "b")

    case a:
      of ("a", "c"):
        fail()
      of ("a", "c"):
        fail()
      of ("a", @c):
        check(c == "b")
      else:
        fail()

  test "ref":
    type
      Node = ref object
        name: string
        children: seq[Node]

    let node = Node(name: "2")

    case node:
      of (name: @name):
        check(name == "2")
      else:
        fail()

    let node2: Node = nil

    case node2:
      of (isNil: false, name: "4"):
        fail()
      else:
        check(true)

  test "weird integers":
    let a = 4

    case [a]:
      of [4'i8]:
        check(true)
      else:
        fail()

  test "dot access":
    let a = Rectangle(b: 4)

    case a:
      of (b: == a.b):
        check(true)
      else:
        fail()

  test "arrays":
    let a = [1, 2, 3, 4]

    case a:
      of [1, @a, 3, @b, 5]:
        fail()
      of [1, @a, 3, @b]:
        check(a == 2 and b == 4)
      else:
        fail()

  test "bool":
    let a = Rectangle(a: 0, b: 0)

    if a.matches((b: 0)):
      check(true)
    else:
      fail()
