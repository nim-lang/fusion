import std/[strutils, sequtils, strformat,
            macros, options, tables, json, algorithm]

import fusion/matching
{.experimental: "caseStmtMacros".}

import unittest

template assertEq(a, b: untyped): untyped =
  if a != b:
    echo a
    echo b
    raiseAssert("Comparison failed in " & $instantiationInfo())

template fail(str: string): untyped =
  echo "Fail on ", instantiationInfo(), ": ", str
  fail()

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

      let node = quote: (12 .. 33)

      block:
        case node:
          of Par [_]:
            discard
          else:
            raiseAssert("#[ IMPLEMENT ]#")

        case node:
          of Par[_]:
            discard
          else:
            raiseAssert("#[ IMPLEMENT ]#")

      block:
        node.assertMatch(Par [_] | Par [_])
        node.assertMatch(Par[Infix()])
        node.assertMatch(Par [Infix()])
        node.assertMatch(Par[Infix ()])
        node.assertMatch(Par [Infix ()])

        node.assertMatch(Par [Infix ()])

      block:

        (
          Par     [Infix [_, @lhs, @rhs]] |
          Command [Infix [_, @lhs, @rhs]] |
          Infix   [@infixId, @lhs, @rhs]
        ) := node

        assert lhs is NimNode
        assert rhs is NimNode
        assert infixId is Option[NimNode]
        assert lhs == newLit(12)
        assert rhs == newLit(33)

      block:
        discard node.matches(
          Call[
            BracketExpr[@ident, opt @outType],
            @body
          ] |
          Command[
            @ident is Ident(),
            Bracket[@outType],
            @body
          ]
        )

        assert body is NimNode, $typeof(body)
        assert ident is NimNode, $typeof(ident)
        assert outType is Option[NimNode]

      block:
        case node:
          of Call[BracketExpr[@ident, opt @outType], @body] |
             Command[@ident is Ident(), Bracket [@outType], @body]
            :
            static:
              assert ident is NimNode, $typeof(ident)
              assert body is NimNode, $typeof(body)
              assert outType is Option[NimNode], $typeof(outType)

          of Call[@head is Ident(), @body]:
            static:
              assert head is NimNode
              assert body is NimNode

    main()

    # block: [0 .. 2 @a is 12] := [12, 12, 12]; assert a == @[12, 12, 12]
    # block:
    #   assert not ([0 .. 2 is 12] ?= [1, 2, 3])
    #   assert not ([(0 .. 2) is 12] ?= [1, 2, 3])

  test "Pattern parser broken brackets":
    block: JArray[@a, @b] := %*[1, 3]
    block: (JArray [@a, @b]) := %*[1, 3]
    block: (JArray [@a, @b is JString()]) := %[%1, %"hello"]
    block: (JArray [@a, @b is JString ()]) := %[%1, %"hello"]
    block: (JArray [
      @a, @b is JString (getStr: "hello")]) := %[%1, %"hello"]

    block:
      let vals = @[
        %*[["AA", "BB"], "CC"],
        %*["AA", ["BB"], "CC"]
      ]

      template testTypes() {.dirty.} =
        assert aa is JsonNode
        assert bb is Option[JsonNode]
        assert cc is JsonNode

        assert aa == %"AA"
        if Some(@bb) ?= bb: assert bb == %"BB"
        assert cc == %"CC"


      for val in vals:
        case val:
          of JArray[JArray[@aa, opt @bb], @cc] |
             JArray[@aa, JArray[@bb], @cc]
            :
            testTypes()
          else:
            fail($val)

        block:
          val.assertMatch(
            JArray[JArray[@aa, opt @bb], @cc] |
            JArray[@aa, JArray[@bb], @cc]
          )

          testTypes()

        block:
          val.assertMatch:
            JArray[JArray[@aa, opt @bb], @cc] |
            JArray[@aa, JArray[@bb], @cc]

          testTypes()

        block:
          val.assertMatch:
            JArray [ JArray [@aa, opt @bb] , @cc] |
            JArray [@aa, JArray [@bb], @cc]

          testTypes()

        block:
          val.assertMatch:
            JArray [
              JArray [
                @aa,
                opt @bb
              ] ,
              @cc
            ] |
            JArray [@aa, JArray [
              @bb], @cc]

          testTypes()


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

    case (1, 2):
      of (3, 4), (1, 2):
        discard
      else:
        fail()


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


    [all _(it < 10)] := [1,2,3,5,6]
    [all < 10] := [1,2,3,4]
    [all (len: < 10)] := [@[1,2,3,4], @[1,2,3,4]]
    [all _.startsWith("--")] := @["--", "--", "--=="]

    block: [@a.startsWith("--")] := ["--12"]

    proc exception() =
      # This should generate quite nice exception message:

      # Match failure for pattern 'all _.startsWith("--")' expected
      # all elements to match, but item at index 2 failed
      [all _.startsWith("--")] := @["--", "--", "=="]

    expect MatchError:
      exception()

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
  func `[]`(node: HtmlNode, idx: int): HtmlNode =
    node.subn[idx]

  func len(n: HtmlNode): int = n.subn.len


  test "Tree builder custom type":

    discard makeTree(HtmlNode, Base())
    discard makeTree(HtmlNode, base())
    discard makeTree(HtmlNode, base([link()]))
    discard makeTree(HtmlNode):
      base:
        link(text: "hello")

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


  test "Tree construction sequence operators":
    block:
      let inTree = makeTree(HtmlNode):
        base:
          link(text: "link1")
          link(text: "link2")

      inTree.assertMatch:
        base:
          all @elems

      let inTree3 = makeTree(HtmlNode):
        base:
          all @elems

      assertEq inTree3, inTree



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

    block:
      block: (fld: @val) := (fld: 12); assert val == 12
      block: (@val, _) := (12, 2); assert val == 12
      block:
        (@val, @val) := (12, 12); assert val == 12
        block: assert (@a, @a) ?= (12, 12)
        block: assert not ((@a, @a) ?= (12, 3))

      block:
        assert [_, _] ?= [12, 2]
        assert not ([_, _] ?= [12, 2, 2])

      block:
        assert [_, .._] ?= [12]
        assert not ([_, _, .._] ?= [12])

      block:
        [_, all @val] := [12, 2, 2]; assert val == @[2, 2]

        # Note that
        block:
          # Does not work, because `assert` internally uses `if` and
          # all variables declared inside are not accesible to the
          # outside scope
          assert [_, all @val] ?= [12, 2, 2]
          when false: # NOTE - will not compile
            assert val == @[2, 2]

    block:
      [until @val is 12, _] := [2, 13, 12]
      assert val == @[2, 13]

    block:
      [until @val is 12, @val] := [2, 13, 12]
      assert val == @[2, 13, 12]


  test "Generic types":
    type
      GenKind = enum
        ptkToken
        ptkNterm

      Gen[Kind, Lex] = ref object
        kindFld: Kind
        case tkind*: GenKind
          of ptkNterm:
            subnodes*: seq[Gen[Kind, Lex]]
          of ptkToken:
            lex*: Lex

    func add[K, L](g: var Gen[K, L], t: Gen[K, L]) =
      g.subnodes.add t

    func kind[K, L](g: Gen[K, L]): K = g.kindFld

    block:
      type
        Kind1 = enum
          k1_val
          k2_val
          k3_val

      const kTokens = {k1_val, k2_val}
      block:
        k1_val(lex: @lex) := Gen[Kind1, string](
          tkind: ptkToken,
          kindFld: k1_val,
          lex: "Hello"
        )

      func `kind=`(g: var Gen[Kind1, string], k: Kind1) =
        if k in kTokens:
          g = Gen[Kind1, string](kindFld: k, tkind: ptkToken)
        else:
          g = Gen[Kind1, string](kindFld: k, tkind: ptkNterm)


      let tree = makeTree(Gen[Kind1, string]):
        k3_val:
          k2_val(lex: "Hello")
          k1_val(lex: "Nice")

    block:
      (lex: @lex) := Gen[void, string](tKind: ptkToken, lex: "hello")






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
    if false: discard (hello3[0][1][2].len: < 10) ?= val3
    assert (hello3[0][0][0].len: < 10) ?= val3
    assert (hello3: is [[[(len: < 10)]]]) ?= val3

  test "Match failure exceptions":
    try:
      [all 12] := [2,3,4]
    except MatchError:
      let msg = getCurrentExceptionMsg()
      assert "all 1" in msg
      assert "all elements" in msg


    expect MatchError:
      [any 1] := [2,3,4]

    try:
      [any 1] := [2,3,4]
    except MatchError:
      let msg = getCurrentExceptionMsg()
      assert "any 1" in msg

    [any is (1 | 2)] := [1, 2]
    try:
      [_, any is (1 | 2)] := [3,4,5]
      fail("_, any is (1 | 2)")
    except MatchError:
      let msg = getCurrentExceptionMsg()
      assert "any is (1 | 2)" in msg
      assert "[any is (1 | 2)]" notin msg

    expect MatchError:
      [none is 12] := [1, 2, 12]

    expect MatchError:
      [_, _, _] := [1, 2]

    try:
      [_, _, _] := [1, 2]
    except MatchError:
      assert "range '3 .. 3'" in getCurrentExceptionMsg()

    try:
      [_, opt _] := [1, 2, 3]
    except MatchError:
      assert "range '1 .. 2'" in getCurrentExceptionMsg()


    try:
      [(1 | 2)] := [3]
      fail("[(1 | 2)] := [3]")
    except MatchError:
      assert "pattern '(1 | 2)'" in getCurrentExceptionMsg()


    1 := 1

    expect MatchError:
      1 := 2

    expect MatchError:
      (1, 2) := (2, 1)

    expect MatchError:
      (@a, @a) := (2, 3)

    expect MatchError:
      (_(it < 12), 1) := (14, 1)


  test "Compilation errors":
    # NOTE that don't know how to correctly test compilation errors,
    # /without/ actuall failing compilation, so I just set `when true`
    # things to see if error is correct

    when false: # Invalid field. NOTE - I'm not sure whether this
      # should be allowed or not, so for now it is disabled. But
      # technically this should not be that hard to allow explicit
      # function calls as part of path expressions.

      # Error: Malformed path access - expected either field name, or
      # bracket access, but found 'fld.call()' of kind nnkCall
      (fld.call(): _) := 12


  test "Use in templates":
    template match1(a: typed): untyped =
      [@nice, @hh69] := a

    match1([12, 3])

    assert nice == 12
    assert hh69 == 3


  type
    Root = ref object of RootObj
      fld1: int
      fld2: float

    SubRoot = ref object of Root
      fld3: int


  test "Ref object field matching":
    case (fld3: 12):
      of (fld3: @subf):
        discard
      else:
        fail()

    var tmp: Root = SubRoot(fld3: 12)
    assert tmp.SubRoot().fld3 == 12
    case tmp:
      of of SubRoot(fld3: @subf):
        assert subf == 12
      else:
        fail()

  test "Ref object in maps, subfields and sequences":
    block:
      @[SubRoot(), Root()].assertMatch([any of SubRoot()])

    block:
      @[SubRoot(fld1: 12), Root(fld1: 33)].assertMatch(
        [all of Root(fld1: @vals)])

      assertEq vals, @[12, 33]

    block:
      let val = {12 : SubRoot(fld1: 33)}.toTable()

      val.assertMatch({
        12 : of SubRoot(fld1: @fld1)
      })


      val.assertMatch({
        12 : of Root(fld1: fld1)
      })

      assertEq fld1, 33



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
    let a = @[
      Rectangle(a: 2, b: 4),
      Rectangle(a: 4, b: 4),
      Rectangle(a: 4, b: 4)
    ]

    case a:
      of []:
        fail()
      of [_, all @others is (a: 4, b: 4)]:
        check(others == a[1 .. ^1])
      else:
        fail()

    # _ is always true, (a: 4, b: 4) didn't match element 2

    # _ is alway.. a.a was 4, but a.b wasn't 4 => not a match

    block:
      [until @vals == 5, .._] := @[2, 3, 4, 5]
      assert vals == @[2, 3, 4]



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

    # WARNING multiple calls for `tokens`. It might be possible to
    # wrap each field access into macro helper that determines
    # whether or not expressions is a function, or just regular
    # field access, and cache execution results, but right now I
    # have no idea how to implement this without redesing of the
    # whole pattern matching construction (and even /with it/ I'm
    # not really sure). So the thing is - expression like
    when false:
      (tokens: [@token, @token]) ?= Email()
    # Internallt access `tokens` multiple times - to get number of
    # elements, and each element. E.g. assumption was made that
    # `obj.tokens` is a cheap expression to evaluate. But in this
    # case we get
    when false:
      let expr_25420001 = Email_25095022()
      block failBlock:
        var pos_25420002 = 0
        if not contains({2..2}, len(tokens(expr_25420001))): # Call to `tokens`
          break failBlock
        ## lkPos @token
        ## Set variable token vkRegular
        if token == tokens(expr_25420001)[pos_25420002]: # Second call
          true
        # ...
        ## lkPos @token
        ## Set variable token vkRegular
        if token == tokens(expr_25420001)[pos_25420002]: # Third call
          true
        # ...

    # Access `tokens` for different indices - `pos_25420002`. Using
    # intermediate variable is not an option, since this would create
    # copies each time, for each field access. But! this is not that
    # big of an issue if we can `lent` everything, so this can
    # probably be solved when view types become more stable. And no,
    # it is not possible to determien whether or not `tokens` is a
    # field or not, since pattern matching DSL does not have
    # information about structure of the object being matched - this
    # is one of the main assumptions that was made, so changing this
    # is not possible without complete redesign and severe cuts in
    # functionality.


    # Note that above this just what I think at the moment, I would be
    # glad if someone told me I'm missing something.


    case email:
      of (data: (name: "academy")):
        fail()

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

suite "More tests":
  test "AST-AST conversion using pattern matching":
    type
      Ast1Kind = enum
        akFirst1
        akSecond1
        akThird1

      Ast1 = object
        case kind1: Ast1Kind
          of akFirst1:
            first: string
          of akSecond1:
            second: int
          of akThird1:
            third: seq[Ast1]

      Ast2Kind = enum
        akFirst2
        akSecond2
        akThird2

      Ast2 = object
        case kind2: Ast2Kind
          of akFirst2:
            first: string
          of akSecond2:
            second: int
          of akThird2:
            third: seq[Ast2]

    func `kind=`(a1: var Ast1, k: Ast1Kind) = a1 = Ast1(kind1: k)
    func `kind=`(a2: var Ast2, k: Ast2Kind) = a2 = Ast2(kind2: k)

    func kind(a1: Ast1): Ast1Kind = a1.kind1
    func kind(a2: Ast2): Ast2Kind = a2.kind2

    func add(a1: var Ast1, sub: Ast1) = a1.third.add sub
    func add(a2: var Ast2, sub: Ast2) = a2.third.add sub

    func convert(a1: Ast1): Ast2 =
      case a1:
        of First1(first: @value):
          return makeTree(Ast2):
            First2(first: value & "Converted")
        of Second1(second: @value):
          return makeTree(Ast2):
            Second2(second: value + 12)
        of Third1(third: @subnodes):
          return makeTree(Ast2):
            Third2(third: == subnodes.map(convert))

    let val = makeTree(Ast1):
      Third1:
        First1(first: "Someval")
        First1(first: "Someval")
        First1(first: "Someval")
        Second1(second: 12)

    discard val.convert()

when false:
  [1 .. 6] := @[1111,1,1,1,1,1,1]
  [1 .. 6 is _]

  # TODO support reverse index for array pattern matching

suite "Article examples":
  test "Small parts":
    let txt = """
root:x:0:0::/root:/bin/bash
bin:x:1:1::/:/usr/bin/nologin
daemon:x:2:2::/:/usr/bin/nologin
mail:x:8:12::/var/spool/mail:/usr/bin/nologin
"""
    for line in txt.strip().split("\n"):
      [@username, 1 .. 6 is _, @shell] := line.split(":")

    block:
      [@a, _] := "A|B".split("|")
      assert a is string
      assert a == "A"

    block:
      let it: seq[string] = "A|B".split("|")
      [@a.startsWith("A"), .._] := it
      assert a is string
      assert a == "A"

    block:
      (1, 2) | (@a, _) := (12, 3)

      assert a is Option[int]

    macro test1(inBody: untyped): untyped =
      block:
        Call[BracketExpr[@ident, opt @outType], @body] := inBody
        assert ident is NimNode
        assert outType is Option[NimNode]
        assert body is NimNode

      block:
        Command[@ident is Ident(), Bracket[@outType], @body] := inBody
        assert ident is NimNode
        assert outType is NimNode
        assert body is NimNode

      block:
        Call[BracketExpr[@ident, opt @outType], @body] |
        Command[@ident is Ident(), Bracket[@outType], @body] := inBody

        assert ident is NimNode
        assert outType is Option[NimNode]
        assert body is NimNode

      block:
        var a = quote do:
          map[string]

        a = a[0]
        a.assertMatch:
          BracketExpr:
            @head
            @typeParam

        assert head.strVal() == "map"
        assert typeParam.strVal() == "string"



  test "Flow macro":
    type
      FlowStageKind = enum
        fskMap
        fskFilter
        fskEach

      FlowStage = object
        outputType: Option[NimNode]
        kind: FlowStageKind
        body: NimNode


    func identToKind(id: NimNode): FlowStageKind =
      if id.eqIdent("map"):
        fskMap
      elif id.eqIdent("filter"):
        fskFilter
      elif id.eqIdent("each"):
        fskEach
      else:
        raiseAssert("#[ IMPLEMENT ]#")

    proc rewrite(node: NimNode, idx: int): NimNode =
      case node:
        of Ident(strVal: "it"):
          result = ident("it" & $idx)
        of (kind: in nnkTokenKinds):
          result = node
        else:
          result = newTree(node.kind)
          for subn in node:
            result.add subn.rewrite(idx)

    func makeTypeAssert(
      expType, body, it: NimNode): NimNode =
      let
        bodyLit = body.toStrLit().strVal().strip().newLit()
        pos = body.lineInfoObj()
        ln = newLit((filename: pos.filename, line: pos.line))

      return quote do:
        when not (`it` is `expType`):
          static:
            {.line: `ln`.}: # To get correct line number when `error`
                            # is used it is necessary to use
                            # `{.line.}` pragma.
              error "\n\nExpected type " & $(typeof(`expType`)) &
                ", but expression \e[4m" & `bodyLit` &
                "\e[24m has type of " & $(typeof(`it`))


    func evalExprFromStages(stages: seq[FlowStage]): NimNode =
      block:
        var expr = stages[^1].body
        if Some(@expType) ?= stages[^1].outputType:
#          ^^             ^^
#          |              |
#          |              Pattern matching operator to determine whether
#          |              right part matches pattern on the left.
#          |
#          Special support for matching `Option[T]` types -

          let assrt = makeTypeAssert(expType, expr, expr)
          # If type assertion is not `none` add type checking.

          expr = quote do:
            `assrt`
            `expr`

      result = newStmtList()
      for idx, stage in stages:
        # Rewrite body
        let body = stage.body.rewrite(idx)


        case stage.kind:
          # If stage is a filter it is converted into `if` expression
          # and new new variables are injected.
          of fskFilter:
            result.add quote do:
              let stageOk = ((`body`))
              if not stageOk:
                continue

          of fskEach:
            # `each` has no variables or special formatting - just
            # rewrite body and paste it back to resulting code
            result.add body
          of fskMap:
            # Create new identifier for injected node and assign
            # result of `body` to it.
            let itId = ident("it" & $(idx + 1))
            result.add quote do:
              let `itId` = `body`

            # If output type for stage needs to be explicitly checked
            # create type assertion.
            if Some(@expType) ?= stage.outputType:
              result.add makeTypeAssert(expType, stage.body, itId)



    func typeExprFromStages(stages: seq[FlowStage], arg: NimNode): NimNode =
      let evalExpr = evalExprFromStages(stages)
      var resTuple = nnkPar.newTree(ident "it0")

      for idx, stage in stages:
        if stage.kind notin {fskFilter, fskEach}:
          resTuple.add ident("it" & $(idx + 1))

      let lastId = newLit(stages.len - 1)

      result = quote do:
        block:
          (
            proc(): auto = # `auto` annotation allows to derive type
                           # of the proc from any assingment withing
                           # proc body - we take advantage of this,
                           # and avoid building type expression
                           # manually.
              for it0 {.inject.} in `arg`:
                `evalExpr`
                result = `resTuple`
#               ^^^^^^^^^^^^^^^^^^^
#               |
#               Type of the return will be derived from this assinment.
#               Even though it is placed within loop body, it will still
#               derive necessary return type
          )()[`lastId`]
#          ^^^^^^^^^^^^
#          | |
#          | Get last element from proc return type
#          |
#          After proc is declared we call it immediatey


    macro flow(arg, body: untyped): untyped =
      # Parse input DSl into sequence of `FlowStage`
      var stages: seq[FlowStage]
      for elem in body:
        if elem.matches(
            Call[BracketExpr[@ident, opt @outType], @body] |
            # `map[string]:`
            Command[@ident is Ident(), Bracket [@outType], @body] |
            # `map [string]:`
            Call[@ident is Ident(), @body]
            # just `map:`, without type argument
          ):
            stages.add FlowStage(
              kind: identToKind(ident),
              outputType: outType,
              body: body
            )

      # Create eval expression
      let evalExpr = evalExprFromStages(stages)

      if stages[^1].kind notin {fskEach}:
        # If last stage has return type (not `each`) then we need to
        # accumulate results in temporary variable.
        let resExpr = typeExprFromStages(stages, arg)
        let lastId = ident("it" & $stages.len)
        let resId = ident("res")
        result = quote do:
          var `resId`: seq[typeof(`resExpr`)]

          for it0 {.inject.} in `arg`:
            `evalExpr`
            `resId`.add `lastid`

          `resId`
      else:
        result = quote do:
          for it0 {.inject.} in `arg`:
            `evalExpr`


      result = newBlockStmt(result)

    let data = """
root:x:0:0:root:/root:/bin/bash
bin:x:1:1:bin:/bin:/sbin/nologin
daemon:x:2:2:daemon:/sbin:/sbin/nologin
adm:x:3:4:adm:/var/adm:/sbin/nologin
lp:x:4:7:lp:/var/spool/lpd:/sbin/nologin
sync:x:5:0:sync:/sbin:/bin/sync
shutdown:x:6:0:shutdown:/sbin:/sbin/shutdown
halt:x:7:0:halt:/sbin:/sbin/halt
mail:x:8:12:mail:/var/spool/mail:/sbin/nologin
news:x:9:13:news:/etc/news:
uucp:x:10:14:uucp:/var/spool/uucp:/sbin/nologin
operator:x:11:0:operator:/root:/sbin/nologin
games:x:12:100:games:/usr/games:/sbin/nologin
gopher:x:13:30:gopher:/var/gopher:/sbin/nologin
ftp:x:14:50:FTP User:/var/ftp:/sbin/nologin
nobody:x:99:99:Nobody:/:/sbin/nologin
nscd:x:28:28:NSCD Daemon:/:/sbin/nologin"""

    let res = flow data.split("\n"):
      map:
        it.split(":")
      filter:
        let shell = it[^1]
        it.len > 1 and shell.endsWith("bash")
      map:
        shell

    assert res is seq[string]
