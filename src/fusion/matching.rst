.. raw:: html
  <blockquote><p>
  "you can probably make a macro for that" -- Rika, 22-09-2020 10:41:51
  </p></blockquote>

:Author: haxscramper

This module implements pattern matching for objects, tuples,
sequences, key-value pairs, case and derived objects. DSL can also be
used to create object trees (AST).

Use example
===========


.. code:: nim

    case [(1, 3), (3, 4)]:
      of [(1, _), _]: 1
      else: 999

Quick reference
===============

============================= =======================================================
 Example                       Explanation
============================= =======================================================
 ``(fld: @val)``               Field ``fld`` into variable ``@val``
 ``Kind()``                    Object with ``.kind == Kind()`` [1]_
 ``of Derived()``              Match object of derived type
 ``(@val, _)``                 First element in tuple in ``@val``
 ``(@val, @val)``              Tuple with two equal elements
 ``{"key" : @val}``            Table with "key", capture into ``@val`` [2]_
 ``[_, _]``                    Sequence with ``len == 2`` [3]_
 ``[_, .._]``                  At least one element
 ``[_, all @val]``             All elements starting from index ``1``
 ``[until @val == "2", .._]``  Capture all elements *until* first ``"2"`` [4]_
 ``[until @val == 1, @val]``   All *including* first match
 ``[all @val == 12]``          All elements are ``== 12``, capture into ``@val``
 ``[some @val == 12]``         At least *one* is ``== 12``, capture all matching into ``@val``
============================= =======================================================

- [1] Kind fields can use shorted enum names - both ``nnkStrLit`` and
  ``StrLit`` will work (prefix ``nnk`` can be omitted)
- [2] Or any object with ``contains`` and ``[]`` defined (for necessary types)
- [3] Or any object with ``len`` proc or field
- [4] Note that sequence must mathc *fully* and it is necessary to have
  ``.._`` at the end in order to accept sequences of arbitrary length.

Supported match elements
========================

- *seqs* - matched using ``[Patt1(), Patt2(), ..]``. Must have
  ``len(): int`` and ``[int]: T`` defined.
- *tuples* - matched using ``(Patt1(), Patt2(), ..)``.
- *pairable* - matched using ``{Key: Patt()}``. Must have ``[Key]: T``
  defined. ``Key`` is not a pattern - search for whole collection
  won't be performed.
- *set* - matched using ``{Val1, Val2, .._}``. Must have ``contains``
  defined. If variable is captured then ``Val1`` must be comparable
  and collection should also implement ``items`` and ``incl``.
- *object* - matched using ``(field: Val)``. Case objects are matched
  using ``Kind(field: Val)``. If you want to check agains multiple
  values for kind field ``(kind: in SomeSetOfKinds)``

Element access
==============

To determine whether or not particular object matches pattern *access
path* is generated - sequence of fields and ``[]`` operators that you
would normally write by hand, like ``fld.subfield["value"].len``. Due to
support for `method call syntax
<https://nim-lang.org/docs/manual.html#procedures-method-call-syntax>`_
there is no difference between field acccess and proc call, so things
like `(len: < 12)` also work as expected.

``(fld: "3")`` Match field ``fld`` against ``"3"``. Generated access
    is ``expr.fld == "3"``.

``["2"]`` Match first element of expression agains patt. Generate
    acess ``expr[pos] == "2"``, where ``pos`` is an integer index for
    current position in sequence.

``("2")`` For each field generate access using ``[1]``

``{"key": "val"}`` First check ``"key" in expr`` and then
    ``expr["key"] == "val"``. No exception on missing keys, just fail
    match.

It is possible to have mixed assess for objects. Mixed object access
via ``(gg: _, [], {})`` creates the same code for checking. E.g ``([_])``
is the same as ``[_]``, ``({"key": "val"})`` is is identical to just
``{"key": "val"}``. You can also call functions and check their values
(like ``(len: _(it < 10))`` or ``(len: in {0 .. 10})``) to check for
sequence length.

Checks
======

- Any operators with exception of ``is`` (subpattern) and ``of`` (derived
  object subpattern) is considered final comparison and just pasted as-is
  into generated pattern match code. E.g. ``fld: in {2,3,4}`` will generate
  ``expr.fld in {2,3,4}``

- ``(fld: is Patt())`` - check if ``expr.fld`` matches pattern ``Patt()``

- ``(fld: _.matchesPredicate())`` - if call to
  ``matchesPredicate(expr.fld)`` evaluates to true.

Notation: ``<expr>`` refers to any possible combination of checks. For
example

- ``fld: in {1,2,3}`` - ``<expr>`` is ``in {1,2,3}``
- ``[_]`` - ``<expr>`` is ``_``
- ``fld: is Patt()`` - ``<expr>`` is ``is Patt()``

Examples
--------

- ``(fld: 12)`` If rhs for key-value pair is integer, string or
  identifier then ``==`` comparison will be generated.
- ``(fld: == ident("33"))`` if rhs is a prefix of ``==`` then ``==`` will
  be generated. Any for of prefix operator will be converted to
  ``expr.fld <op> <rhs>``.
- ``(fld: in {1, 3, 3})`` or ``(fld: in Anything)`` creates ``fld.expr
  in Anything``. Either ``in`` or ``notin`` can be used.

Variable binding
================

Match can be bound to new varaible. All variable declarations happen
via ``@varname`` syntax.

- To bind element to variable without any additional checks do: ``(fld: @varname)``
- To bind element with some additional operator checks do:

  - ``(fld: @varname <operator> Value)`` first perform check using
    ``<operator>`` and then add ``Value`` to ``@varname``
    - ``(fld: @hello is ("2" | "3"))``

- Predicate checks: ``fld: @a.matchPredicate()``
- Arbitrary expression: ``fld: @a(it mod 2 == 0)``. If expression has no
  type it is considered ``true``.

Bind order
----------

Bind order: if check evaluates to true variable is bound immediately,
making it possible to use in other checks. ``[@head, any @tail !=
head]`` is a valid pattern. First match ``head`` and then any number
of ``@tail`` elements. Can use ``any _(if it != head: tail.add it)``
and declare ``tail`` externally.

Variable is never rebound. After it is bound, then it will have the
value of first binding.

Bind variable type
------------------

- Any variadics are mapped to sequence
- Only once in alternative is option
- Explicitly optional is option
- Optional with default value is regular value
- Variable can be used only once if in alternative


========================== =====================================
 Pattern                     Ijected variables
========================== =====================================
 ``[@a]``                    ``var a: typeof(expr[0])``
 ``{"key": @val}``           ``var val: typeof(expr["key"])``
 ``[all @a]``                ``var a: seq[typeof(expr[0])]``
 ``[opt @val]``              ``var a: Option[typeof(expr[0])]``
 ``[opt @val or default]``   ``var a: typeof(expr[0])``
 ``(fld: @val)``             ``var val: typeof(expr.fld)``
========================== =====================================

Matching different things
=========================

Sequence matching
-----------------

Input sequence: ``[1,2,3,4,5,6,5,6]``

================================= ======================== ====================================
 Pattern                           Result                   Comment
================================= ======================== ====================================
 ``[_]``                           **Fail**                 Input sequence size mismatch
 ``[.._]``                         **Ok**
 ``[@a]``                          **Fail**                 Input sequence size mismatch
 ``[@a, .._]``                     **Ok**, ``a = 1``
 ``[any @a, .._]``                 **Error**
 ``[any @a(it < 10)]``             **Ok**, ``a = [1..6]``   Capture all elements that match
 ``[until @a == 6, .._]``          **Ok**                   All until first ocurrence of ``6``
 ``[all @a == 6, .._]``            **Ok** ``a = []``        All leading ``6``
 ``[any @a(it > 100)]``            **Fail**                 No elements ``> 100``
 ``[none @a(it in {6 .. 10})]``    **Fail**                 There is an element ``== 6``
================================= ======================== ====================================

``until``
    non-greedy. Match everything until ``<expr>``

    - ``until <expr>``: match all until frist element that matches Expr

``all``
    greedy. Match everything that matches ``<expr>``

    - ``all <expr>``: all elements should match Expr

    - ``all @val is <expr>``: capture all elements in ``@val`` if ``<expr>``
      is true for every one of them.
``opt``
    Single element match

    - ``opt @a``: match optional element and bind it to a

    - ``opt @a or "default"``: either match element to a or set a to
      "default"
``any``
    greedy. Consume all sequence elements until the end and
    succed only if any element has matched.

    - ``any @val is "d"``: capture all element that match ``is "d"``

``none``
    greedy. Consume all sequence elements until the end and
    succed only if any element has matched. EE

``[m .. n @capture]``
    Capture slice of elements from index `m` to `n`

Greedy patterns match until the end of a sequence and cannot be
followed by anything else.

For sequence to match is must either be completely matched by all
subpatterns or have trailing ``.._`` in pattern.

============= ============== ==============
 Sequence      Pattern        Match result
============= ============== ==============
 ``[1,2,3]``   ``[1,2]``      **Fail**
               ``[1, .._]``   **Ok**
               ``[1,2,_]``    **Ok**
============= ============== ==============

More use examples

- capture all elements in sequence: ``[all @elems]``
- get all elements until (not including "d"): ``[until @a is "d"]``
- All leading "d": ``[all @leading is "d"]``
- Match first two elements and ignore the rest ``[_, _, .._]``
- Match optional third element ``[_, _, opt @trail]``
- Match third element and if not matched use default value ``[_, _,
  opt @trail or "default"]``
- Capture all elements until first separator: ``[until @leading is
  "sep", @middle is "sep", all @trailing]``
- Extract all conditions from IfStmt: ``IfStmt([all ElseIf([@cond,
  _]), .._])``


How different sequence matching keywords map to regular for-loops?

.. code:: nim

    # all
    var allOk = false
    while position < len:
      if matchExpr(): inc position
      else: allOk = false; break

    if not allOk: # Fail match

.. code:: nim

    # any
    var foundOk = false
    while position < len:
      if matchExpr(): foundOk = true
      inc position

    if not foundOk: # Fail match


.. code:: nim

    # until
    var foundOk = false
    while position < len:
      if not matchExpr(): break
      else: inc position

    # Continue with next matches

Tuple matching
--------------

Input tuple: ``(1, 2, "fa")``

============================ ========== ============
 Pattern                      Result      Comment
============================ ========== ============
 ``(_, _, _)``                **Ok**      Match all
 ``(@a, @a, _)``              **Fail**
 ``(@a is (1 | 2), @a, _)``   **Error**
 ``(1, 1 | 2, _)``            **Ok**
============================ ========== ============

Case object matching
--------------------

Input AST

.. code:: nim

    ForStmt
      Ident "i"
      Infix
        Ident ".."
        IntLit 1
        IntLit 10
      StmtList
        Command
          Ident "echo"
          IntLit 12

- ``ForStmt([== ident("i"), .._])`` Only for loops with ``i`` as
  variable
- ``ForStmt([@a is Ident(), .._])`` Capture for loop variable
- ``ForStmt([@a.isTuple(), .._])`` for loops in which first subnode
  satisfies predicate ``isTuple()``. Bind match to ``a``
- ``ForStmt([_, _, (len: in {1 .. 10})])`` between one to ten
  statements in the for loop body

Ref object matching
-------------------

Matching for ref objects is not really different from regular one - the
only difference is that you need to use ``of`` operator explicitly. For
example, if you want to do ``case`` match for different object kinds - and

.. code:: nim

    case Obj():
      of of StmtList(subfield: @capture):
        # do something with `capture`

You can use ``of`` as prefix operator - things like ``{12 : of
SubRoot(fld1: @fld1)}``, or  ``[any of Derived()]``.


KV-pairs matching
-----------------

Input json string

.. code:: json

    {"menu": {
      "id": "file",
      "value": "File",
      "popup": {
        "menuitem": [
          {"value": "New", "onclick": "CreateNewDoc()"},
          {"value": "Open", "onclick": "OpenDoc()"},
          {"value": "Close", "onclick": "CloseDoc()"}
        ]
      }
    }}

- Get input ``["menu"]["file"]`` from node and

.. code:: nim
    case inj:
      of {"menu" : {"file": @file is JString()}}:
        # ...
      else:
        raiseAssert("Expected [menu][file] as string, but found " & $inj)

Option matching
---------------

``Some(@x)`` and ``None()`` is a special case that will be rewritten into
``(isSome: true, get: @x)`` and ``(isNone: true)`` respectively. This is
made to allow better integration with optional types.  [9]_ .

Tree construction
=================

``makeTree`` provides 'reversed' implementation of pattern matching,
which allows to *construct* tree from pattern, using variables.
Example of use

.. code:: nim

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

    discard makeTree(HtmlNode):
      base:
        link(text: "hello")

In order to construct tree, ``kind=`` and ``add`` have to be defined.
Internally DSL just creats resulting object, sets ``kind=`` and then
repeatedly ``add`` elements to it. In order to properties for objects
either the field has to be exported, or ``fld=`` has to be defined
(where ``fld`` is the name of property you want to set).
