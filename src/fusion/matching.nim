import sequtils, macros, tables, options, strformat, strutils,
       parseutils, algorithm, hashes

# TODO handle malformed input pattern with `@var or "default"` in
# sequences/kv-pairs. Should be written as `opt @var or "default"`

## .. include:: matching.rst


const
  nnkStrKinds* = { nnkStrLit .. nnkTripleStrLit }
  nnkIntKinds* = { nnkCharLit .. nnkUInt64Lit }
  nnkFloatKinds* = { nnkFloatLit .. nnkFloat128Lit }
  nnkTokenKinds* = nnkStrKinds + nnkIntKinds + nnkFloatKinds + {
    nnkIdent,
    nnkSym
  }

template `->`(a, b: bool): bool = (if a: b else: true)

template getSome[T](opt: Option[T], injected: untyped): bool =
  opt.isSome() and ((let injected {.inject.} = opt.get(); true))

template assertKind(node: NimNode, kindSet: set[NimNodeKind]): untyped =
  if node.kind notin kindSet:
    raiseAssert("Expected one of " & $kindSet & " but node has kind " &
      $node.kind & " (assertion on " & $instantiationInfo() & ")")

func nodeStr(n: NimNode): string =
  case n.kind:
    of nnkIdent: n.strVal()
    of nnkOpenSymChoice: n[0].strVal()
    else: raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")

func startsWith(n: NimNode, str: string): bool =
  n.nodeStr().startsWith(str)


func idxTreeRepr(inputNode: NimNode, maxLevel: int = 120): string =
  func aux(node: NimNode, parent: seq[int]): seq[string] =
    result.add parent.mapIt(&"[{it}]").join("") &
      "  ".repeat(6) &
      ($node.kind)[3..^1] &
      (if node.len == 0: " " & node.toStrLit().strVal() else: "")

    for idx, subn in node:
      if parent.len + 1 < maxLevel:
        result &= aux(subn, parent & @[idx])
      else:
        result &= (parent & @[idx]).mapIt(&"[{it}]").join("") &
          " ".repeat(6 + 3 + 3)  & "[...] " & ($subn.kind)[3..^1]

  return aux(inputNode, @[]).join("\n")


func parseEnumField(fld: NimNode): string =
  ## Get name of enum field from nim node
  case fld.kind:
    of nnkEnumFieldDef:
      fld[0].strVal
    of nnkSym:
      fld.strVal
    else:
      raiseAssert(&"#[ IMPLEMENT {fld.kind} ]#")

func parseEnumImpl(en: NimNode): seq[string] =
  ## Get sequence of enum value names
  case en.kind:
    of nnkSym:
      let impl = en.getTypeImpl()
      case impl.kind:
        of nnkBracketExpr:
          return parseEnumImpl(impl.getTypeInst()[1].getImpl())
        of nnkEnumTy:
          result = parseEnumImpl(impl)
        else:
          assertKind(impl, {nnkBracketExpr, nnkEnumTy})
          # raiseAssert(&"#[ IMPLEMENT {impl.kind} ]#")
    of nnkTypeDef:
      result = parseEnumImpl(en[2])
    of nnkEnumTy:
      for fld in en[1..^1]:
        result.add parseEnumField(fld)
    of nnkTypeSection:
      result = parseEnumImpl(en[0])
    else:
      raiseAssert(&"#[ IMPLEMENT {en.kind} ]#")


func pref(name: string): string =
  discard name.parseUntil(result, {'A' .. 'Z', '0' .. '9'})

func foldInfix(s: seq[NimNode],
               inf: string, start: seq[NimNode] = @[]): NimNode =
  ( start & s ).mapIt(it.newPar().newPar()).foldl(
    nnkInfix.newTree(ident inf, a, b))


func commonPrefix(strs: seq[string]): string =
  ## Find common prefix for seq of strings
  if strs.len == 0:
    return ""
  else:
    let strs = strs.sorted()
    for i in 0 ..< min(strs[0].len, strs[^1].len):
      if strs[0][i] == strs[^1][i]:
        result.add strs[0][i]
      else:
        return


func dropPrefix(str: string, alt: string): string =
  if str.startsWith(alt):
    return str[min(alt.len, str.len)..^1]
  return str

func dropPrefix(ss: seq[string], patt: string): seq[string] =
  for s in ss:
    result.add s.dropPrefix(patt)


template findItFirstOpt*(s: typed, op: untyped): untyped =
  var res: Option[typeof(s[0])]
  for it {.inject.} in s:
    if op:
      res = some(it)
      break

  res


func addPrefix*(str, pref: string): string =
  if not str.startsWith(pref): pref & str else: str

func hash(iinfo: LineInfo): Hash =
  !$(iinfo.line.hash !& iinfo.column.hash !& iinfo.filename.hash)

proc getKindNames*(head: NimNode): (string, seq[string]) =
  var
    pref: string
    names: seq[string]
    cache {.global.}: Table[LineInfo, (string, seq[string])]

  block:
    let
      impl = head.getTypeImpl()
      iinfo = impl.lineInfoObj()

    if iinfo notin cache:
      let
        decl = impl.parseEnumImpl()
        pref = decl.commonPrefix().pref()

      cache[iinfo] = (pref, decl.dropPrefix(pref))

    pref = cache[iinfo][0]
    names = cache[iinfo][1]

  return (pref, names)



macro hasKindImpl*(head: typed, kind: untyped): untyped =
  # TODO validate correcness of pattern in `kind` - if prefixed
  # identifier is not valid enum name, report immediately.
  let (pref, names) = getKindNames(head)
  kind.assertKind({nnkIdent})
  let kind = ident(kind.toStrLit().strVal().addPrefix(pref))
  result = nnkInfix.newTree(ident "==", head, kind)


template hasKind*(head, kindExpr: untyped): untyped =
  ## Determine if `head` has `kind` value. Either function/procedure
  ## `kind` or field with the same name is expecte to be declared.
  ## Type of `kind` must be an enum. Kind expression is a pattern
  ## describing expected values. Possible examples of pattern
  ## (assuming value of type `NimNode` is used as `head`)
  ##
  ## - `nnkIntLit` - match integer literal
  ## - `IntLit` - alternative (preferred) syntax for matching enum values
  ##   `nnk` prefix can be omitted.
  ## - `{IntLit, StrLit}` - check for multiple kinds at the same time
  ## - `{IntLit, +StrLiterals}` - check if kind value is either in an
  ##   integer literal or is contained in set `StrLiterals` (which must)
  ##   be declared externally. THis syntax is useful for checking common
  ##   sets of kind values such as 'integer literals', 'literals' etc.
  ##
  ## NOTE: this template is used internally by `match` macro
  ## implementation - all patterns can also be used to match case
  ## objects (for example `{IntLit, StrLit}()` to match either integer
  ## or string literal node or `{+Literals}()` for matching any
  ## literal node)
  hasKindImpl(head.kind, kindExpr)

type
  MatchKind* = enum
    ## Different kinds of matching patterns
    kItem ## Match single element
    kSeq ## Match sequence of elements
    kTuple ## Mach tuple (anonymous or named)
    kPairs ## Match key-value pairs
    kObject ## Match object, named tuple or object-like value
    kSet ## Match set of elements
    kAlt ## Ordered choice - mactch any of patterns.

  SeqKeyword* = enum
    ## Possible special words for seq pattern matching
    lkAny = "any" ## Any element from seq
    lkAll = "all" ## All elements from seq
    lkNone = "none"  ## None of the elements from seq
    lkOpt = "opt" ## Optionaly match element in seq
    lkUntil = "until" ## All elements until
    lkPref = "pref" ## All elements while
    lkPos ## Exact position
    lkSlice ## Subrange slice
    lkTrail ## Variadic placeholder `.._`

  SeqStructure* = object
    decl: NimNode ## Original declaration of the node
    bindVar*: Option[NimNode] ## Optional bound variable
    patt*: Match ## Patterh for element matching
    case kind*: SeqKeyword
      of lkSlice:
        slice*: NimNode
      else:
        discard

  ItemMatchKind* = enum
    ## Type of item pattern match
    imkInfixEq ## Match item using infix operator
    imkSubpatt ## Match item by checking it agains subpattern
    imkPredicate ## Execute custom predicate to determine if element
                 ## matches pattern.

  KVPair* = object
    key: NimNode
    patt: Match

  Match* = ref object
    ## Object describing single match for element
    bindVar*: Option[NimNode] ## Bound variable (if any)
    declNode*: NimNode ## Original declaration of match
    isOptional*: bool
    fallback*: Option[NimNode] ## Default value in case match fails
    case kind*: MatchKind
      of kItem:
        case itemMatch: ItemMatchKind
          of imkInfixEq:
            infix*: string ## Infix operator used for comparison
            rhsNode*: NimNode ## Rhs expression to compare against
            isPlaceholder*: bool ## Always true? `_` pattern is an
            ## infix expression with `isPlaceholder` equal to true
          of imkSubpatt:
            rhsPatt*: Match ## Subpattern to compare value against
          of imkPredicate:
            isCall*: bool ## Predicate is a call expression
            ## (`@val.matches()`) or a free-standing expression
            ## (`@val(it.len < 100)`)
            predBody*: NimNode ## Body of the expression

      of kAlt:
        altElems*: seq[Match] ## Alternatives for matching
      of kSeq:
        seqElems*: seq[SeqStructure] ## Sequence subpatterns
      of kTuple:
        tupleElems*: seq[Match] ## Tuple elements
      of kPairs:
        pairElems*: seq[KVPair]

      of kSet:
        setElems*: seq[Match]
      of kObject:
        kindCall*: Option[NimNode] ## Optional node with kind
        ## expression pattern (see `hasKind`)
        fldElems*: seq[tuple[
          name: string,
          patt: Match
        ]]

        kvMatches*: Option[Match] ## Optional key-value matches for
        ## expressions like `JObject({"key": @val})`
        seqMatches*: Option[Match]  ## Optional indexed matches for
        ## subelement access using `Infix([@op, @lhs, @rhs])` pattern.

  AccsElem = object
    isVariadic: bool
    case inStruct: MatchKind
      of kSeq:
        pos*: NimNode ## Expressions for accessing seq element
      of kTuple:
        idx*: int ## Tuple field index
      of kObject:
        fld*: string ## Object field name
      of kPairs:
        key*: NimNode ## Expression for key-value pair
        nocheck*: bool
      of kSet, kAlt:
        discard
      of kItem:
        isOpt*: bool ## Is match optional

  Path = seq[AccsElem]

  VarKind* = enum
    ## Kind of matched variables
    vkRegular ## Regular variable, assigned once
    vkSequence
    vkOption
    vkSet
    vkAlt

  VarSpec* = object
    decl*: NimNode ## First time variable has been declared
    varKind*: VarKind ## Type of the variable
    typePath*: Path ## Whole path for expression that can be used to
                    ## determine type of the variable.

  VarTable = Table[string, VarSpec]

func isNamedTuple(node: NimNode): bool =
  node.allIt(it.kind in {
    nnkExprColonExpr, # `(fld: )`
    nnkBracket, # `([])`
    nnkTableConstr # `{key: val}`
  }) and
  node.allIt((it.kind == nnkIdent) -> (it.strVal == "_"))

func isInfixPatt(node: NimNode): bool =
  node.kind == nnkInfix and
  node[0].kind == nnkIdent and
  node[0].strVal() in ["|"]

func makeVarSet(varn: NimNode, expr: NimNode, vtable: VarTable): NimNode =
  varn.assertKind({nnkIdent})
  case vtable[varn.strVal()].varKind:
    of vkSequence:
      return quote do:
        `varn`.add `expr` ## Append item to sequence

    of vkOption:
      return quote do:
        `varn` = some(`expr`) ## Set optional value

    of vkSet:
      return quote do:
        `varn`.incl some(`expr`) ## Add element to set

    of vkRegular:
      let wasSet = ident(varn.strVal() & "WasSet")
      return quote do:
        if not `wasSet`:
          `varn` = `expr`

    of vkAlt:
      return nnkAsgn.newTree(varn, expr)

func toAccs*(path: Path, name: string): NimNode =
  ## Convert path in object to expression for getting element at path.
  func aux(prefix: NimNode, top: Path): NimNode =
    let head = top[0]
    result = case head.inStruct:
      of kSeq:
        nnkBracketExpr.newTree(prefix, top[0].pos)
      of kTuple:
        nnkBracketExpr.newTree(prefix, newLit(top[0].idx))
      of kObject:
        nnkDotExpr.newTree(prefix, ident head.fld)
      of kPairs:
        nnkBracketExpr.newTree(prefix, head.key)
      of kItem, kAlt:
        prefix
      of kSet:
        raiseAssert(
          "Invalid access path: cannot create explicit access for set")

    if top.len > 1:
      result = result.aux(top[1 ..^ 1])


  result =
    if path.len > 0:
      (ident name).aux(path)
    else:
      ident name


func parseMatchExpr*(n: NimNode): Match

func parseKVTuple(n: NimNode): Match =
  if n[0].eqIdent("Some"):
    if not (n.len <= 2):
      error("Expected `Some(@varBind)`", n)

    if not (n[1].kind == nnkPrefix and n[1][0].eqIdent("@")):
      # debugecho n[1].lispRepr()
      error(
        "Some(@var) pattern expected, but found " &
          n[1].toStrLit().strVal(), n[1])

    n[1].assertKind({nnkPrefix})
    n[1][0].assertKind({nnkIdent})

    return Match(kind: kObject, declNode: n, fldElems: @{
      "isSome": Match(kind: kItem, itemMatch: imkInfixEq, declNode: n[0],
                      rhsNode: newLit(true), infix: "=="),
      # TODO generate compile assertions for 'get' imports? E.g. it is
      # not really clear where error has originated if it is just
      # regular shit-tier error like `no get identifier`
      "get": Match(kind: kItem, itemMatch: imkInfixEq,
                   declNode: n[1], isPlaceholder: true,
                   bindVar: some(n[1][1])),
    })


  result = Match(kind: kObject, declNode: n)
  var start = 0
  if n.kind in {nnkCall, nnkObjConstr}:
    start = 1
    result.kindCall = some(n[0])

  for elem in n[start .. ^1]:
    case elem.kind:
      of nnkExprColonExpr:
        elem[0].assertKind({nnkIdent})
        result.fldElems.add((
          elem[0].strVal(),
          elem[1].parseMatchExpr()))
      of nnkBracket, nnkStmtList:
        result.seqMatches = some(elem.parseMatchExpr())
      of nnkTableConstr:
        result.kvMatches = some(elem.parseMatchExpr())
      else:
        elem.assertKind({nnkExprColonExpr})

func contains(kwds: openarray[SeqKeyword], str: string): bool =
  for kwd in kwds:
    if eqIdent($kwd, str):
      return true

func parseSeqMatch(n: NimNode): seq[SeqStructure] =
  # TEST `all is {+Set}(val: @lines)` and make sure it is the same as
  # `all {+Set}(val: @lines)`. Handle prefixes uniformly
  for elem in n:
    if elem.kind == nnkPrefix and elem[0].eqIdent(".."):
      elem[1].assertKind({nnkIdent})
      result.add SeqStructure(kind: lkTrail, patt: Match(
        declNode: elem
      ), decl: elem)
    elif
      # `[0 .. 3 @head is Jstring()]`
      (elem.kind == nnkInfix and (elem[0].startsWith(".."))) or
      # `[(0 .. 3) @head is Jstring()]`
      (elem.kind == nnkCommand and elem[0].kind == nnkPar) or
      # `[0 .. 2 is 12]`
      (elem.kind == nnkInfix and
       elem[1].kind == nnkInfix and
       elem[1][0].startsWith("..")
      ):

      var dotInfix, rangeStart, rangeEnd, body: NimNode

      if elem.kind == nnkInfix:
        if elem.kind == nnkInfix and elem[1].kind == nnkInfix:
          # `0 .. 2 is 12`
          #             Infix
          # [0]            Ident is
          # [1]            Infix
          # [1][0]            [...] Ident
          # [1][1]            [...] IntLit
          # [1][2]            [...] IntLit
          # [2]            IntLit 12
          dotInfix = elem[1][0]
          rangeStart = elem[1][1]
          rangeEnd = elem[1][2]
          body = elem[2]
        else:
          # `0 .. 2 @a is 12`
          #             Infix
          # [0]            Ident ..
          # [1]            IntLit 0
          # [2]            Command
          # [2][0]            IntLit 2
          # [2][1]            Infix
          # [2][1][0]            [...] Ident
          # [2][1][1]            [...] Prefix
          # [2][1][2]            [...] IntLit
          dotInfix = ident elem[0].nodeStr()
          rangeStart = elem[1]
          rangeEnd = elem[2][0]
          body = elem[2][1]

      elif elem.kind == nnkCommand:
        # I wonder, why do we need pattern matching in stdlib?
        dotInfix = ident elem[0][0][0].nodeStr()
        rangeStart = elem[0][0][1]
        rangeEnd = elem[0][0][1]
        body = elem[1]
      # elif elem.kind == nnkInfix and :

      var res = SeqStructure(
        kind: lkSlice, slice: nnkInfix.newTree(
          dotInfix,
          rangeStart,
          rangeEnd
        ),
        patt: parseMatchExpr(body),
        decl: elem
      )

      res.bindVar = res.patt.bindVar
      res.patt.bindVar = none(NimNode)
      result.add res

      # debugecho elem.treeRepr()
    else:
      func toKwd(node: NimNode): SeqKeyword =
        for (key, val) in {
          "any" : lkAny,
          "all" : lkAll,
          "opt" : lkOpt,
          "until" : lkUntil,
          "none" : lkNone,
          "pref" : lkPref
            }:
          if node.eqIdent(key):
            result = val
            break


      var (elem, opKind) = (elem, lkPos)
      let seqKwds = [lkAny, lkAll, lkNone, lkOpt, lkUntil, lkPref]
      if elem.kind in {nnkCall, nnkCommand} and
         elem[0].kind notin {nnkDotExpr} and
         elem[0].strVal() in seqKwds:
        opKind = toKwd(elem[0])
        elem = elem[1]
      elif elem.kind in {nnkInfix} and
           elem[1].kind in {nnkIdent} and
           elem[1].strVal() in seqKwds:
        opKind = toKwd(elem[1])
        elem = nnkInfix.newTree(elem[0], ident "_", elem[2])

      var
        match = parseMatchExpr(elem)
        bindv = match.bindVar

      match.bindVar = none(NimNode)
      match.isOptional = opKind in {lkOpt}

      var it = SeqStructure(bindVar: bindv, kind: opKind, decl: elem)
      it.patt = match
      result.add(it)

func parseTableMatch(n: NimNode): seq[KVPair] =
  for elem in n:
    result.add(KVPair(
      key: elem[0],
      patt: elem[1].parseMatchExpr()
    ))

func parseAltMatch(n: NimNode): Match =
  let
    lhs = n[1].parseMatchExpr()
    rhs = n[2].parseMatchExpr()

  var alts: seq[Match]
  if lhs.kind == kAlt: alts.add lhs.altElems else: alts.add lhs
  if rhs.kind == kAlt: alts.add rhs.altElems else: alts.add rhs
  result = Match(kind: kAlt, altElems: alts, declNode: n)

func splitOpt(n: NimNode): tuple[
  lhs: NimNode, rhs: Option[NimNode]] =

  n[0].assertKind({nnkIdent})
  if not n[0].eqIdent("opt"):
    error("Only `opt` is supported for standalone item matches", n[0])

  if not n.len == 2:
    error("Expected exactly one parameter for `opt`", n)

  if n[1].kind == nnkInfix:
    result.lhs = n[1][1]
    result.rhs = some n[1][2]
  else:
    result.lhs = n[1]


func parseMatchExpr*(n: NimNode): Match =
  ## Parse match expression from nim node
  case n.kind:
    of nnkIdent, nnkSym, nnkIntLit, nnkStrLit, nnkCharLit:
      result = Match(kind: kItem, itemMatch: imkInfixEq, declNode: n)
      if n == ident "_":
        result.isPlaceholder = true
      else:
        result.rhsNode = n
        result.infix = "=="
    of nnkPar: # Named or unnamed tuple
      if n.isNamedTuple(): # `(fld1: ...)`
        result = parseKVTuple(n)
      elif n[0].isInfixPatt(): # `(12 | 3)`
        result = parseAltMatch(n[0])
      else: # Unnamed tuple `( , , , , )`
        result = Match(kind: kTuple, declNode: n)
        for elem in n:
          result.tupleElems.add parseMatchExpr(elem)
    of nnkPrefix: # `is Patt()`, `@capture` or other prefix expression
      if n[0].nodeStr() == "is": # `is Patt()`
        result = Match(
          kind: kItem, itemMatch: imkSubpatt,
          rhsPatt: parseMatchExpr(n[1]), declNode: n)

      elif n[0].nodeStr() == "@": # `@capture`
        n[1].assertKind({nnkIdent})
        result = Match(
          kind: kItem, itemMatch: imkInfixEq, isPlaceholder: true,
          bindVar: some(n[1]), declNode: n)

      else: # Other prefix expression, for example `== 12`
        result = Match(
          kind: kItem, itemMatch: imkInfixEq, infix: n[0].strVal(),
          rhsNode: n[1], declNode: n
        )

    of nnkBracket, nnkStmtList:
      # `[1,2,3]` - seq pattern in inline form or as seq of elements
      # (stmt list)
      result = Match(
        kind: kSeq, seqElems: parseSeqMatch(n), declNode: n)
    of nnkTableConstr: # `{"key": "val"}` - key-value matches
      result = Match(
        kind: kPairs, pairElems: parseTableMatch(n), declNode: n)
    of nnkCurly: # `{1, 2}` - set pattern
      result = Match(kind: kSet, declNode: n)
      for node in n:
        if node.kind in {nnkExprColonExpr}:
          error("Unexpected colon", node) # TODO:DOC


        result.setElems.add parseMatchExpr(node)
    of nnkBracketExpr:
      result = Match(
        kindCall: some n[0],
        kind: kObject,
        declNode: n,
        seqMatches: some parseMatchExpr(
          nnkBracket.newTree(n[1..^1])
        )
      )
    elif n.kind in {nnkObjConstr, nnkCall, nnkCommand} and
         not n[0].eqIdent("opt"):
      if n[0].kind == nnkPrefix:
        n[0][1].assertKind({nnkIdent}) # `@capture(<some-expression>)`
        result = Match(
          kind: kItem,
          itemMatch: imkPredicate,
          bindVar: some(n[0][1]),
          declNode: n,
          predBody: n[1]
        )
      elif n[0].kind == nnkDotExpr: # `_.call("Arguments")`
        # `(DotExpr (Ident "_") (Ident "<function-name>"))`
        n[0][1].assertKind({nnkIdent})
        n[0][0].assertKind({nnkIdent})
        var body = n
        # Replace `_` with `it` to make `it.call("arguments")`
        body[0][0] = ident("it")
        result = Match(
          kind: kItem,
          itemMatch: imkPredicate,
          declNode: n,
          predBody: body
        )
      else:
        result = parseKVTuple(n)
    elif (n.kind in {nnkCommand, nnkCall}) and n[0].eqIdent("opt"):
      let (lhs, rhs) = splitOpt(n)
      result = lhs.parseMatchExpr()
      result.isOptional = true
      result.fallback = rhs
    elif n.isInfixPatt(): # `(true, true) | (false, false)`
      result = parseAltMatch(n)
    elif n.kind == nnkInfix:
      n[1].assertKind({nnkPrefix, nnkIdent})
      if n[1].kind in {nnkPrefix}:
        n[1][1].assertKind({nnkIdent})

      if n[0].strVal() == "is":
        # `@patt is JString()`
        # `@head is 'd'`
        result = Match(
          kind: kItem, itemMatch: imkSubpatt,
          rhsPatt: parseMatchExpr(n[2]), declNode: n)

      else:
        # `@a | @b`, `@a == 6`
        result = Match(
          kind: kItem, itemMatch: imkInfixEq,
          rhsNode: n[2],
          infix: n[0].strVal(), declNode: n)

        if result.infix == "or":
          result.isOptional = true
          result.fallback = some n[2]

      if n[1].kind == nnkPrefix: # WARNING
        # debugecho n.idxTreeRepr()
        result.bindVar = some(n[1][1])
    else:
      raiseAssert(&"#[ IMPLEMENT for kind {n.kind} ]#")

func isVariadic(p: Path): bool = p.anyIt(it.isVariadic)

func isAlt(p: Path): bool = p.anyIt(it.inStruct == kAlt)

func isOption(p: Path): bool =
  p.anyIt(it.inStruct == kItem and it.isOpt)

func classifyPath(path: Path): VarKind =
  if path.isVariadic:
    vkSequence
  elif path.isAlt:
    vkAlt
  elif path.isOption:
    vkOption
  else:
    vkRegular

func addvar(tbl: var VarTable, vsym: NimNode, path: Path): void =
  # TODO disallow setting variable again if it has already
  # participated in speculative match.
  let vs = vsym.strVal()
  # echov path
  if vs notin tbl:
    tbl[vs] = VarSpec(
      decl: vsym,
      varKind: path.classifyPath(),
      typePath: path
    )
  else:
    let
      class = path.classifyPath()
      update = (class == vkSequence) or
        (class == vkOption and tbl[vs].varKind in {vkRegular})

    if update:
      tbl[vs].varKind = class
      tbl[vs].typePath = path

func makeVarTable(m: Match): VarTable =
  func aux(sub: Match, vt: var VarTable, path: Path): void =
    if sub.bindVar.getSome(bindv):
      # echov sub.rhsNode
      if sub.isOptional and sub.fallback.isNone():
        # echov "opt"
        vt.addvar(bindv, path & @[
          AccsElem(inStruct: kItem, isOpt: true)
        ])
      else:
        vt.addVar(bindv, path)

    case sub.kind:
      of kItem, kSet:
        discard
      of kAlt:
        for alt in sub.altElems:
          aux(alt, vt, path & @[AccsElem(inStruct: kAlt)])
      of kSeq:
        # echov path
        # echov sub[]
        for elem in sub.seqElems:
          # echov elem
          let parent = path & @[AccsElem(
            inStruct: kSeq, pos: newLit(0),
            isVariadic: elem.kind notin {lkPos, lkOpt})]

          if elem.bindVar.getSome(bindv):
            if elem.patt.isOptional and elem.patt.fallback.isNone():
              vt.addVar(bindv, parent & @[
                AccsElem(inStruct: kItem, isOpt: true)
              ])
            else:
              vt.addVar(bindv, parent)

          aux(elem.patt, vt, parent)

      of kTuple:
        for idx, it in sub.tupleElems:
          aux(it, vt, path & @[AccsElem(inStruct: kTuple, idx: idx)])
      of kPairs:
        for pair in sub.pairElems:
          aux(pair.patt, vt, path & @[AccsElem(inStruct: kPairs, key: pair.key)])
      of kObject:
        for (fld, patt) in sub.fldElems:
          aux(patt, vt, path & @[AccsElem(inStruct: kObject, fld: fld)])

        if sub.seqMatches.getSome(seqm):
          aux(seqm, vt, path)

        if sub.kvMatches.getSome(kv):
          aux(kv, vt, path)


  aux(m, result, @[])
  # debugPprint result


func makeMatchExpr*(
  m: Match, vtable: VarTable; path: Path, mainExpr: string): NimNode

template makeElemMatch(): untyped {.dirty.} =
  case elem.kind:
    of lkPos:
      inc minLen
      inc maxLen
      if elem.bindVar.getSome(bindv):
        result.add makeVarSet(bindv, parent.toAccs(mainExpr), vtable)
        # vtable.addvar(bindv, parent) # XXXX

      if elem.patt.kind == kItem and
         elem.patt.itemMatch == imkInfixEq and
         elem.patt.isPlaceholder:
        result.add newCall(ident "inc", posid)
      else:
        result.add quote do:
          if `expr`:
            inc `posid`
          else:
            `failBreak`

    else:
      maxLen = 5000
      var varset = newEmptyNode()

      if elem.bindVar.getSome(bindv):
        varset = makeVarSet(bindv, parent.toAccs(mainExpr), vtable)
        # vtable.addvar(bindv, parent) # XXXX

      case elem.kind:
        of lkAll:
          let allOk = genSym(nskVar, "allOk")
          var check: NimNode
          if expr.kind in {nnkSym, nnkIdent} and expr.eqIdent("true"):
            check = quote do:
              `varset`
          else:
            check = quote do:
              if not `expr`:
                `allOk` = false
              else:
                `varset`

          result.add quote do:
            block:
              var `allOk`: bool = true
              while `posid` < `getLen` and `allOk`:
                `check`
                inc `posid`

              if not `allOk`:
                break `failBlock`

        of lkSlice:
          var rangeExpr = elem.slice
          result.add quote do:
            for tmp in `rangeExpr`:
              `posid` = tmp
              if `posid` < `getLen` and `expr`:
                `varset`
              else:
                break `failBlock`

        of lkUntil:
          result.add quote do:
            while (`posid` < `getLen`) and (not `expr`):
              `varset`
              inc `posid`

          if idx == seqm.seqElems.len - 1:
            result.add quote do:
              if (`posid` < `getLen`): ## Not full match
                break `failBlock`

        of lkAny:
          result.add quote do:
            block:
              var foundOk: bool = false
              while `posid` < `getLen`:
                if `expr`:
                  foundOk = true
                  `varset`

                inc `posid`

              if not foundOk:
                break `failBlock`
        of lkPref:
          result.add quote do:
            while `posid` < `getLen` and `expr`:
              `varset`
              inc `posid`
        of lkOpt:
          var default = nnkDiscardStmt.newTree(newEmptyNode())
          if elem.patt.isOptional and
             elem.bindVar.getSome(bindv) and
             elem.patt.fallback.getSome(fallback):

            default = makeVarSet(bindv, fallback, vtable)
          result.add quote do:
            if `posid` < `getLen`:
              `varset`
              inc `posid`
            else:
              `default`
        of lkNone:
          let allOk = genSym(nskVar, "allOk")
          var check: NimNode

          if expr.kind in {nnkSym, nnkIdent} and expr.eqIdent("true"):
            check = quote do:
              `varset`
          else:
            check = quote do:
              if `expr`:
                `allOk` = false
              else:
                `varset`

          result.add quote do:
            block:
              var `allOk`: bool = true
              while `posid` < `getLen` and `allOk`:
                `check`
                inc `posid`

              if not `allOk`:
                break `failBlock`

        of lkTrail, lkPos:
          # ???
          discard



func makeSeqMatch(
  seqm: Match, vtable: VarTable; path: Path,
  mainExpr: string): NimNode =
  var idx = 1
  while idx < seqm.seqElems.len:
    if seqm.seqElems[idx - 1].kind notin {
      lkUntil, lkPos, lkOpt, lkPref, lkSlice}:
      error("Greedy seq match must be last element in pattern",
            seqm.seqElems[idx].decl)

    inc idx

  let
    posid = genSym(nskVar, "pos")
    matched = genSym(nskVar, "matched")
    failBlock = ident("failBlock")
    failBreak = nnkBreakStmt.newTree(failBlock)
    getLen = newCall("len", path.toAccs(mainExpr))


  result = newStmtList()
  var minLen = 0
  var maxLen = 0
  for idx, elem in seqm.seqElems:
    if elem.kind == lkTrail:
      maxLen = 5000
    else:
      let
        parent = path & @[AccsElem(
          inStruct: kSeq, pos: posid,
          isVariadic: elem.kind notin {lkPos, lkOpt})]

        expr = elem.patt.makeMatchExpr(vtable, parent, mainExpr)


      result.add newCommentStmtNode(
        $elem.kind & " " & elem.patt.declNode.repr)

      makeElemMatch()

  let
    comment = newCommentStmtNode(seqm.declNode.repr)
    minNode = newLit(minLen)
    maxNode = newLit(maxLen)
    setCheck =
      if maxLen >= 5000:
        quote do:
          `getLen` < `minNode`
      else:
        quote do:
          `getLen` notin {`minNode` .. `maxNode`}

  result = quote do:
    `comment`
    var `matched` = false
    block `failBlock`:
      var `posid` = 0 ## Start seq match

      if `setCheck`:
        ## fail on seq len
        break `failBlock`

      `result`

      `matched` = true ## Seq match ok

    `matched`

  result = result.newPar().newPar()




func makeMatchExpr*(
  m: Match, vtable: VarTable; path: Path, mainExpr: string): NimNode =
  ## Create NimNode for checking whether or not item referred to by
  ## `mainExpr` matches pattern described by `Match`
  case m.kind:
    of kItem:
      let parent = path.toAccs(mainExpr)
      case m.itemMatch:
        of imkInfixEq, imkSubpatt:
          let inf =
            if m.itemMatch == imkInfixEq:
              if m.isPlaceholder:
                newLit(true)
              else:
                nnkInfix.newTree(ident m.infix, parent, m.rhsNode)
             else:
               makeMatchExpr(m.rhsPatt, vtable, path, mainExpr)

          if m.bindVar.getSome(vname):
            # vtable.addvar(vname, path) # XXXX
            let bindVar = makeVarSet(vname, parent, vtable)
            if inf == newLit(true):
              return quote do:
                (`bindVar`; true)
            else:
              return quote do:
                block:
                  if `inf`:
                    `bindVar`
                    true
                  else:
                    false
          else:
            return inf
        of imkPredicate:
          let pred = m.predBody
          var bindVar = newEmptyNode()
          if m.bindVar.getSome(vname):
            # vtable.addvar(vname, path) # XXXX
            bindVar = makeVarSet(vname, parent, vtable)

          result = quote do:
            let it {.inject.} = `parent`
            if `pred`:
              `bindVar`
              true
            else:
              false


    of kSeq:
      return makeSeqMatch(m, vtable, path, mainExpr)
    of kTuple:
      var conds: seq[NimNode]
      for idx, it in m.tupleElems:
        conds.add it.makeMatchExpr(vtable, path & @[
          AccsElem(inStruct: kTuple, idx: idx)
        ],  mainExpr)

      return conds.foldInfix("and")
    of kObject:
      var conds: seq[NimNode]
      if m.kindCall.getSome(kc):
        conds.add newCall(ident "hasKind", path.toAccs(mainExpr), kc)

      for (fld, patt) in m.fldElems:
        conds.add patt.makeMatchExpr(vtable, path & @[
          AccsElem(inStruct: kObject, fld: fld)],  mainExpr)

      if m.seqMatches.getSome(seqm):
        conds.add seqm.makeMatchExpr(vtable, path,  mainExpr)

      if m.kvMatches.getSome(kv):
        conds.add kv.makeMatchExpr(vtable, path,  mainExpr)

      return conds.foldInfix("and")

    of kPairs:
      var conds: seq[NimNode]
      for pair in m.pairElems:
        let
          accs = path.toAccs(mainExpr)
          incheck = nnkInfix.newTree(ident "in", pair.key, accs)
          valPath = path & @[AccsElem(inStruct: kPairs, key: pair.key)]
          valGet = valPath.toAccs(mainExpr)

        if not pair.patt.isOptional:
          conds.add nnkInfix.newTree(
            ident "and", incheck,
            pair.patt.makeMatchExpr(vtable, valPath, mainExpr)
          )

        else:
          let varn = pair.patt.bindVar.get
          let varsetOk = makeVarSet(varn, valGet, vtable)
          if pair.patt.fallback.getSome(fallback):
            let varsetFail = makeVarSet(
              varn, pair.patt.fallback.get(), vtable)

            conds.add quote do:
              block:
                if `incheck`:
                  `varsetOk`
                else:
                  `varsetFail`

                true
          else:
            conds.add quote do:
              if `incheck`:
                `varsetOk`

              true




      return conds.foldInfix("and")
    of kAlt:
      var conds: seq[NimNode]
      for alt in m.altElems:
        conds.add alt.makeMatchExpr(
          vtable, path & @[AccsElem(inStruct: kAlt)],  mainExpr)

      return conds.foldInfix("or")
    of kSet:
      var conds: seq[NimNode]
      let setPath = path.toAccs(mainExpr)
      for elem in m.setElems:
        if elem.kind == kItem and elem.infix == "==":
          conds.add nnkInfix.newTree(ident "in", elem.rhsNode, setPath)
        else:
          error(
            "Only `contains` check are supported for sets",
            elem.declNode
          )

      return conds.foldInfix("and")


func makeMatchExpr(m: Match, mainExpr: string): tuple[
    expr: NimNode, vtable: VarTable] =
  result.vtable = makeVarTable(m)
  result.expr = makeMatchExpr(m, result.vtable, @[],  mainExpr)

func toNode(
  input: tuple[expr: NimNode, vtable: VarTable],
  mainExpr: string): NimNode =
  var (expr, vtable) = input

  var exprNew = nnkStmtList.newTree()
  for name, spec in vtable:
    let vname = ident(name)
    var typeExpr = toAccs(spec.typePath, mainExpr)
    typeExpr = quote do:
      ((let tmp = `typeExpr`; tmp))

    case spec.varKind:
      of vkSequence:
        exprNew.add quote do:
          var `vname`: seq[typeof(`typeExpr`)]

      of vkOption:
        exprNew.add quote do:
          var `vname`: Option[typeof(`typeExpr`)]

      of vkSet, vkAlt:
        exprNew.add quote do:
          var `vname`: typeof(`typeExpr`)

      of vkRegular:
        let wasSet = ident(vname.strVal() & "WasSet")
        exprNew.add quote do:
          var `wasSet`: bool = false
          var `vname`: typeof(`typeExpr`)

  return quote do:
    `exprNew`
    `expr`

macro expand*(body: typed): untyped = body

macro match*(n: untyped): untyped =
  var matchcase = nnkIfStmt.newTree()
  for elem in n[1 .. ^1]:
    case elem.kind:
      of nnkOfBranch:
        if elem[0] == ident "_":
          error("To create catch-all match use `else` clause", elem[0])


        matchcase.add nnkElifBranch.newTree(
          elem[0].parseMatchExpr().makeMatchExpr( "expr").
            toNode("expr").newPar().newPar(),
          elem[1]
        )

      of nnkElifBranch, nnkElse:
        matchcase.add elem
      else:
        discard

  let head = n[0]

  let pos = newCommentStmtNode($n.lineInfoObj())

  result = quote do:
    block:
      `pos`
      let expr {.inject.} = `head`
      let pos {.inject.}: int = 0
      `matchcase`


macro assertMatch*(input, pattern: untyped): untyped =
  let
    expr = ident genSym(nskLet, "expr").repr
    matched = pattern.parseMatchExpr().
      makeMatchExpr(expr.repr).toNode(expr.repr)


  let patt = newLit(pattern.repr)
  result = quote do:
    let `expr` = `input`
    let ok = `matched`

    if not ok:
      raiseAssert("Pattern match failed `" & `patt` & "`")

  # echo result.toStrLIt()

macro matches*(input, pattern: untyped): untyped =
  let
    expr = ident genSym(nskLet, "expr").repr
    matched = pattern.parseMatchExpr().
      makeMatchExpr(expr.repr).toNode(expr.repr)

  return quote do:
    let `expr` = `input`
    `matched`

func buildTreeMaker(
  prefix: string, resType: NimNode, match: Match): NimNode =

  case match.kind:
    of kItem:
      if (match.itemMatch == imkInfixEq) and (match.infix == "=="):
        if match.isPlaceholder:
          if match.bindVar.getSome(bindv):
            result = newIdentNode(bindv.strVal())
          else:
            error(
              "Only variable placeholders allowed for pattern " &
                "construction",
              match.declNode
            )
        else:
          if match.rhsNode != nil:
            result = match.rhsNode
          else:
            error("Empty rhs node for item", match.declNode)
    of kObject:
      var res = newStmtList()
      let tmp = genSym(nskVar, "res")

      res.add quote do:
        var `tmp`: `resType`
        when `tmp` is ref:
          new(`tmp`)

      if match.kindCall.getSome(call):
        let kind = ident call.strVal().addPrefix(prefix)
        res.add quote do:
          {.push warning[CaseTransition]: off.}
          `tmp`.kind = `kind`
          {.pop.}

      else:
        error("Named tuple construction is not supported", match.declNode)

      for (name, patt) in match.fldElems:
        res.add nnkAsgn.newTree(newDotExpr(
          tmp, ident name
        ), buildTreeMaker(prefix, resType, patt))

      if match.seqMatches.isSome():
        for sub in match.seqMatches.get().seqElems:
          res.add newCall("add", tmp, buildTreeMaker(
            prefix, resType, sub.patt))

      res.add tmp

      result = newBlockStmt(res)
    of kSeq:
      var res = newStmtList()
      let tmp = genSym(nskVar, "res")
      res.add quote do:
        var `tmp`: seq[`resType`]

      for sub in match.seqElems:
        res.add newCall("add", tmp, buildTreeMaker(
          prefix, resType, sub.patt))

      res.add quote do:
        `tmp`

      result = newBlockStmt(res)
    else:
      error(
        &"Pattern of kind {match.kind} is " &
          "not supported for tree construction",
        match.declNode
      )

func `kind=`*(node: var NimNode, kind: NimNodeKind) =
  node = newNimNode(kind, node)

func str*(node: NimNode): string = node.strVal()
func `str=`*(node: var NimNode, val: string) =
  if node.kind in {nnkIdent, nnkSym}:
    node = ident val
  else:
    node.strVal = val

func getTypeIdent(node: NimNode): NimNode =
  case node.getType().kind:
    of nnkObjectTy:
      newCall("typeof", node)
    else:
      node.getType()

macro makeTreeImpl(node, kind: typed, patt: untyped): untyped =
  var inpatt = patt
  if patt.kind in {nnkStmtList}:
    if patt.len > 1:
      raiseAssert("#[ IMPLEMENT wrap in stmt list ]#")
    else:
      inpatt = patt[0]

  let (pref, _) = kind.getKindNames()

  var match = inpatt.parseMatchExpr()
  result = buildTreeMaker(pref, node.getTypeIdent(), match)

  if patt.kind in {nnkStmtList} and
     patt[0].len == 1 and
     match.kind == kSeq and
     patt[0].kind notin {nnkBracket}
    :
    result = nnkBracketExpr.newTree(result, newLit(0))

template makeTree*(T: typed, patt: untyped): untyped =
  ## Construct tree from pattern matching expression. For example of
  ## use see documentation at the start of the module
  block:
    var tmp: T
    makeTreeImpl(tmp, tmp.kind, patt)

template `:=`*(lhs, rhs: untyped): untyped =
  ## Shorthand for `assertMatch`
  assertMatch(rhs, lhs)

template `?=`*(lhs, rhs: untyped): untyped =
  ## Shorthand for `matches`
  matches(rhs, lhs)
