import sequtils, macros, tables, options, strformat, strutils,
       parseutils, algorithm, hashes

export options

## .. include:: matching.rst

const
  nnkStrKinds* = {
    nnkStrLit .. nnkTripleStrLit
  } ## Set of all nim node kinds for string nodes

  nnkIntKinds* = {
    nnkCharLit .. nnkUInt64Lit
  } ## Set of all nim node kinds for integer literal nodes

  nnkFloatKinds* = {
    nnkFloatLit .. nnkFloat128Lit
  } ## Set of all nim node kinds for float literal nodes

  nnkIdentKinds* = {
    nnkIdent, nnkSym, nnkOpenSymChoice
  } ## Set of all nim node kinds for identifier-like nodes

  nnkTokenKinds* =
    nnkStrKinds + nnkIntKinds + nnkFloatKinds +
    nnkIdentKinds ## Set of all token-like nodes (primitive type
                  ## literals or identifiers)

func codeFmt(str: string): string =
  &"\e[4m{str}\e[24m"

func codeFmt(node: NimNode): NimNode =
  node.strVal().codeFmt().newLit()

func nodeStr(n: NimNode): string =
  ## Get nim node string value from any identifier or string literal node
  case n.kind:
    of nnkIdent: n.strVal()
    of nnkOpenSymChoice: n[0].strVal()
    of nnkSym: n.strVal()
    of nnkStrKinds: n.strVal()
    else: raiseAssert(&"Cannot get string value from node kind {n.kind}")

func lineIInfo(node: NimNode): NimNode =
  ## Create tuple literal for `{.line: .}` pragma
  let iinfo = node.lineInfoObj()
  newLit((filename: iinfo.filename, line: iinfo.line))



func idxTreeRepr(inputNode: NimNode, maxLevel: int = 120): string =
  func aux(node: NimNode, parent: seq[int]): seq[string] =
    result.add parent.mapIt(&"[{it}]").join("") &
      "  ".repeat(6) &
      ($node.kind)[3..^1] &
      (if node.len == 0: " " & node.toStrLit().nodeStr() else: "")

    for idx, subn in node:
      if parent.len + 1 < maxLevel:
        result &= aux(subn, parent & @[idx])
      else:
        result &= (parent & @[idx]).mapIt(&"[{it}]").join("") &
          " ".repeat(6 + 3 + 3)  & "[...] " & ($subn.kind)[3..^1]

  return aux(inputNode, @[]).join("\n")



template `->`(a, b: bool): bool = (if a: b else: true)

template getSome[T](opt: Option[T], injected: untyped): bool =
  opt.isSome() and ((let injected {.inject.} = opt.get(); true))

func splitDots(n: NimNode): seq[NimNode] =
  result = case n.kind:
    of nnkDotExpr:
      if n[0].kind == nnkDotExpr:
        splitDots(n[0]) & @[n[1]]
      elif n[0].kind == nnkBracketExpr:
        splitDots(n[0]) & splitDots(n[1])
      else:
        @[n[0], n[1]]
    of nnkBracketExpr:
      if n[0].kind in nnkIdentKinds:
        @[n[0]] & splitDots(n[1]).mapIt(nnkBracket.newTree(it))
      else:
        n[0][0].splitDots() & (
          n[0][1].splitDots() & n[1].splitDots()
        ).mapIt(nnkBracket.newTree(it))
    else:
      @[n]

func getVar(n: NimNode): NimNode =
  if n.kind == nnkPragmaExpr:
    n[1][0][1]
  elif n.kind == nnkPrefix:
    n[1]
  else:
    raiseAssert(&"Cannot get variable from node kind {n.kind}")

func firstDot(n: NimNode): NimNode =
  splitDots(n)[0]

func dropFirstDot(n: NimNode): NimNode =
  let drop = splitDots(n)
  if drop.len == 1:
    drop[0]
  else:
    drop[1..^1].foldl(nnkDotExpr.newTree(a, b))


template assertKind(node: NimNode, kindSet: set[NimNodeKind]): untyped =
  if node.kind notin kindSet:
    raiseAssert("Expected one of " & $kindSet & " but node has kind " &
      $node.kind & " (assertion on " & $instantiationInfo() & ")")

func startsWith(n: NimNode, str: string): bool =
  n.nodeStr().startsWith(str)



func parseEnumField(fld: NimNode): string =
  ## Get name of enum field from nim node
  case fld.kind:
    of nnkEnumFieldDef:
      fld[0].nodeStr
    of nnkSym:
      fld.nodeStr
    else:
      raiseAssert(&"Cannot parse enum field for kind: {fld.kind}")

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
      raiseAssert(&"Cannot parse enum element for kind {en.kind}")


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
  let (pref, names) = getKindNames(head)
  kind.assertKind({nnkIdent})
  let str = kind.toStrLit().nodeStr().addPrefix(pref)
  let kind = ident(str)
  if str.dropPrefix(pref) notin names:
    error("Invalid kind name - " & kind.toStrLit().strVal(), kind)

  result = nnkInfix.newTree(ident "==", head, kind)


template hasKind*(head, kindExpr: untyped): untyped =
  ## Determine if `head` has `kind` value. Either function/procedure
  ## `kind` or field with the same name is expected to be declared.
  ## Type of `kind` must be an enum. Kind expression is a pattern
  ## describing expected values. Possible examples of pattern
  ## (assuming value of type `NimNode` is used as `head`)
  ##
  ## - `nnkIntLit` - match integer literal
  ## - `IntLit` - alternative (preferred) syntax for matching enum values
  ##   `nnk` prefix can be omitted.
  when compiles(head.kind):
    hasKindImpl(head.kind, kindExpr)
  else:
    static: error "No `kind` defined for " & $typeof(head)

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

  MatchError* = ref object of CatchableError ## Exception indicating match failure

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
        nocheck*: bool

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
    cnt*: int ## Number of variable occurencies in expression

  VarTable = Table[string, VarSpec]

func isNamedTuple(node: NimNode): bool =
  node.allIt(it.kind in {
    nnkExprColonExpr, # `(fld: )`
    nnkBracket, # `([])`
    nnkTableConstr # `{key: val}`
  }) and
  node.allIt((it.kind == nnkIdent) -> (it.nodeStr == "_"))

func makeVarSet(
  varn: NimNode, expr: NimNode, vtable: VarTable, doRaise: bool): NimNode =
  varn.assertKind({nnkIdent})
  case vtable[varn.nodeStr()].varKind:
    of vkSequence:
      return quote do:
        `varn`.add `expr` ## Append item to sequence
        true

    of vkOption:
      return quote do:
        `varn` = some(`expr`) ## Set optional value
        true

    of vkSet:
      return quote do:
        `varn`.incl some(`expr`) ## Add element to set
        true

    of vkRegular:
      let wasSet = ident(varn.nodeStr() & "WasSet")
      let varStr = varn.toStrLit()
      let ln = lineIInfo(vtable[varn.nodeStr()].decl)
      let matchError =
        if doRaise:
          quote do:
            when compiles($(`varn`)):
              {.line: `ln`.}:
                raise MatchError(
                  msg: "Match failure: variable '" & `varStr` &
                    "' is already set to " & $(`varn`) &
                    ", and does not match with " & $(`expr`) & "."
                )
            else:
              {.line: `ln`.}:
                raise MatchError(
                  msg: "Match failure: variable '" & `varStr` &
                    "' is already set and new value does not match."
                )
         else:
           quote do:
             discard false


      if vtable[varn.nodeStr()].cnt > 1:
        return quote do:
          if `wasSet`:
            if `varn` == `expr`:
              true
            else:
              if true:
                `matchError`
              false
          else:
            `varn` = `expr`
            `wasSet` = true
            true
      else:
        return quote do:
          `varn` = `expr`
          true

    of vkAlt:
      return quote do: # WARNING
        `varn` = `expr`
        true

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

func parseNestedKey(n: NimNode): Match =
  ## Unparse key-value pair with nested fields. `fld: <pattern>` and
  ## `fld1.subfield.subsubfield: <pattern>`. Lattern one is just
  ## shorthand for `(fld1: (subfield: (subsubfield: <pattern>)))`.
  ## This function returns `(subfield: (subsubfield: <pattern>))` part
  ## - first key should be handled by caller.
  n.assertKind({nnkExprColonExpr})
  func aux(spl: seq[NimNode]): Match =
    case spl[0].kind:
      of nnkIdentKinds:
        if spl.len == 1:
          return n[1].parseMatchExpr()
        else:
          if spl[1].kind in nnkIdentKinds:
            return Match(
              kind: kObject,
              declNode: spl[0],
              fldElems: @[
                (name: spl[1].nodeStr(), patt: aux(spl[1 ..^ 1]))
              ])
          else:
            return Match(
              kind: kPairs,
              declNode: spl[0],
              pairElems: @[KvPair(
                key: spl[1][0], patt: aux(spl[1 ..^ 1]))],
              nocheck: true
            )
      of nnkBracket:
        if spl.len == 1:
          return n[1].parseMatchExpr()
        else:
          if spl[1].kind in nnkIdentKinds:
            return Match(
              kind: kObject,
              declNode: spl[0],
              fldElems: @[
                (name: spl[1].nodeStr(), patt: aux(spl[1 ..^ 1]))
              ])
          else:
            return Match(
              kind: kPairs,
              declNode: spl[1],
              pairElems: @[KvPair(
                key: spl[1][0], patt: aux(spl[1 ..^ 1]))],
              nocheck: true
            )
      else:
        error(
          "Malformed path access - expected either field name, " &
            "or bracket access ['key'], but found " &
            spl[0].toStrLit().strVal() &
            " of kind " & $spl[0].kind,
          spl[0]
        )

  return aux(n[0].splitDots())



func parseKVTuple(n: NimNode): Match =
  if n[0].eqIdent("Some"):
    if not (n.len <= 2):
      error("Expected `Some(@varBind)`", n)

    if not (n[1].kind == nnkPrefix and n[1][0].eqIdent("@")):
      # debugecho n[1].lispRepr()
      error(
        "Some(@var) pattern expected, but found " &
          n[1].toStrLit().nodeStr(), n[1])

    n[1].assertKind({nnkPrefix})
    n[1][0].assertKind({nnkIdent})

    return Match(kind: kObject, declNode: n, fldElems: @{
      "isSome": Match(kind: kItem, itemMatch: imkInfixEq, declNode: n[0],
                      rhsNode: newLit(true), infix: "=="),
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
        var str: string
        case elem[0].kind:
          of nnkIdentKinds, nnkDotExpr:
            str = elem[0].firstDot().nodeStr()
          else:
            error(
              "Malformed path access - expected either field name, " &
                "or bracket access, but found '" &
                elem[0].toStrLit().strVal() & "'" &
                " of kind " & $elem[0].kind,
              elem[0]
            )

        result.fldElems.add((str, elem.parseNestedKey()))
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


      let topElem = elem
      var (elem, opKind) = (elem, lkPos)
      let seqKwds = [lkAny, lkAll, lkNone, lkOpt, lkUntil, lkPref]
      if elem.kind in {nnkCall, nnkCommand} and
         elem[0].kind in {nnkSym, nnkIdent} and
         elem[0].nodeStr() in seqKwds:
        opKind = toKwd(elem[0])
        elem = elem[1]
      elif elem.kind in {nnkInfix} and
           elem[1].kind in {nnkIdent} and
           elem[1].nodeStr() in seqKwds:
        opKind = toKwd(elem[1])
        elem = nnkInfix.newTree(elem[0], ident "_", elem[2])

      var
        match = parseMatchExpr(elem)
        bindv = match.bindVar

      match.bindVar = none(NimNode)
      match.isOptional = opKind in {lkOpt}

      var it = SeqStructure(bindVar: bindv, kind: opKind, decl: topElem)
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

# echo currentSourcePath()

func parseMatchExpr*(n: NimNode): Match =
  ## Parse match expression from nim node
  case n.kind:
    of nnkIdent, nnkSym, nnkIntKinds, nnkStrKinds, nnkFloatKinds:
      result = Match(kind: kItem, itemMatch: imkInfixEq, declNode: n)
      if n == ident "_":
        result.isPlaceholder = true
      else:
        result.rhsNode = n
        result.infix = "=="
    of nnkPar: # Named or unnamed tuple
      if n.isNamedTuple(): # `(fld1: ...)`
        result = parseKVTuple(n)
      elif n[0].kind == nnkInfix and n[0][0].eqIdent("|"):
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
          kind: kItem, itemMatch: imkInfixEq, infix: n[0].nodeStr(),
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
          error("Unexpected colon", node)


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
      elif n[0].kind == nnkDotExpr:
        var body = n
        var bindVar: Option[NimNode]
        if n[0][0].kind == nnkPrefix:
          # debugecho n.idxTreeRepr()
          n[0][0][1].assertKind({nnkIdent})
          bindVar = some(n[0][0][1])

          # Store name of the bound variable and then replace `_` with
          # `it` to make `it.call("arguments")`
          body[0][0] = ident("it")
        else: # `_.call("Arguments")`
          # `(DotExpr (Ident "_") (Ident "<function-name>"))`
          n[0][1].assertKind({nnkIdent, nnkOpenSymChoice})
          n[0][0].assertKind({nnkIdent, nnkOpenSymChoice})

          # Replace `_` with `it` to make `it.call("arguments")`
          body[0][0] = ident("it")

        result = Match(
          kind: kItem,
          itemMatch: imkPredicate,
          declNode: n,
          predBody: body,
          bindVar: bindVar
        )
      elif n.kind == nnkCall and n[0].eqIdent("_"):
        # `_(some < expression)`. NOTE - this is probably a
        # not-that-common use case, but I don't think explicitly
        # disallowing it will make things more intuitive.
        result = Match(
          kind: kItem,
          itemMatch: imkPredicate,
          declNode: n[1],
          predBody: n[1]
        )
      else:
        result = parseKVTuple(n)
    elif (n.kind in {nnkCommand, nnkCall}) and n[0].eqIdent("opt"):
      let (lhs, rhs) = splitOpt(n)
      result = lhs.parseMatchExpr()
      result.isOptional = true
      result.fallback = rhs
    elif n.kind == nnkInfix and n[0].eqIdent("|"):
      # `(true, true) | (false, false)`
      result = parseAltMatch(n)
    elif n.kind in {nnkInfix, nnkPragmaExpr}:
      n[1].assertKind({nnkPrefix, nnkIdent, nnkPragma})
      if n[1].kind in {nnkPrefix}:
        n[1][1].assertKind({nnkIdent})

      if n[0].nodeStr() == "is":
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
          infix: n[0].nodeStr(), declNode: n)

        if result.infix == "or":
          result.isOptional = true
          result.fallback = some n[2]

      if n[1].kind == nnkPrefix: # WARNING
        result.bindVar = some(n[1][1])
    else:
      error(
        "Malformed DSL - found " & n.toStrLit().strVal() &
          " of kind " & $n.kind & ".", n)

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
  let vs = vsym.nodeStr()
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

  inc tbl[vs].cnt

func makeVarTable(m: Match): tuple[table: VarTable,
                                   mixident: seq[string]] =

  func aux(sub: Match, vt: var VarTable, path: Path): seq[string] =
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
      of kItem:
        if sub.itemMatch == imkInfixEq and
           sub.isPlaceholder and
           sub.bindVar.isNone()
          :
          result &= "_"
      of kSet:
        discard
      of kAlt:
        for alt in sub.altElems:
          result &= aux(alt, vt, path & @[AccsElem(inStruct: kAlt)])
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

          result &= aux(elem.patt, vt, parent)

      of kTuple:
        for idx, it in sub.tupleElems:
          result &= aux(it, vt, path & @[
            AccsElem(inStruct: kTuple, idx: idx)])
      of kPairs:
        for pair in sub.pairElems:
          result &= aux(pair.patt, vt, path & @[
            AccsElem(inStruct: kPairs, key: pair.key)])
      of kObject:
        for (fld, patt) in sub.fldElems:
          result &= aux(patt, vt, path & @[
            AccsElem(inStruct: kObject, fld: fld)])

        if sub.seqMatches.getSome(seqm):
          result &= aux(seqm, vt, path)

        if sub.kvMatches.getSome(kv):
          result &= aux(kv, vt, path)


  result.mixident = aux(m, result.table, @[]).deduplicate()
  # debugPprint result


func makeMatchExpr(
  m: Match, vtable: VarTable; path: Path,
  mainExpr: string, doRaise: bool,
  originalMainExpr: string
     ): NimNode

template makeElemMatch(): untyped {.dirty.} =
  let pattStr = newLit(elem.decl.toStrLit().strVal())
  case elem.kind:
    of lkPos:
      inc minLen
      inc maxLen
      let ln = elem.decl.lineIInfo()
      if doRaise:
        var str = newNimNode(nnkRStrLit)
        str.strVal = "Match failure for pattern '" & pattStr.strVal() &
            "'. Item at index "

        failBreak = quote do:
          {.line: `ln`.}:
            raise MatchError(msg: `str` & $(`posid` - 1) & " failed")

      if elem.bindVar.getSome(bindv):
        result.add newCommentStmtNode(
          "Set variable " & bindv.nodeStr() & " " &
            $vtable[bindv.nodeStr()].varKind)


        let vars = makeVarSet(
          bindv, parent.toAccs(mainExpr), vtable, doRaise)
        result.add quote do:
          if not `vars`:
            `failBreak`

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
        varset = makeVarSet(
          bindv, parent.toAccs(mainExpr), vtable, doRaise)
        # vtable.addvar(bindv, parent) # XXXX

      let ln = elem.decl.lineIInfo()
      if doRaise:
        case elem.kind:
          of lkAll:
            failBreak = quote do:
              {.line: `ln`.}:
                raise MatchError(
                  msg: "Match failure for pattern '" & `pattStr` &
                    "' expected all elements to match, but item at index " &
                    $(`posid` - 1) & " failed"
                )

          of lkAny:
            failBreak = quote do:
              {.line: `ln`.}:
                raise MatchError(
                  msg: "Match failure for pattern '" & `pattStr` &
                    "'. Expected at least one elemen to match, but got none"
                )
          of lkNone:
            failBreak = quote do:
              {.line: `ln`.}:
                raise MatchError(
                  msg: "Match failure for pattern '" & `pattStr` &
                    "'. Expected no elements to match, but index " &
                    $(`posid` - 1) & " matched."
                )

          else:
            discard


      case elem.kind:
        of lkAll:
          let allOk = genSym(nskVar, "allOk")
          var check: NimNode
          if expr.kind in {nnkSym, nnkIdent} and expr.eqIdent("true"):
            check = quote do:
              discard `varset`
          else:
            check = quote do:
              if not `expr`:
                `allOk` = false
              else:
                discard `varset`

          result.add quote do:
            block:
              var `allOk`: bool = true
              while `posid` < `getLen` and `allOk`:
                `check`
                inc `posid`

              if not `allOk`:
                `failBreak`

        of lkSlice:
          var rangeExpr = elem.slice
          result.add quote do:
            for tmp in `rangeExpr`:
              `posid` = tmp
              if `posid` < `getLen` and `expr`:
                discard `varset`
              else:
                `failBreak`

        of lkUntil:
          result.add quote do:
            while (`posid` < `getLen`) and (not `expr`):
              discard `varset`
              inc `posid`

          if idx == seqm.seqElems.len - 1:
            result.add quote do:
              if (`posid` < `getLen`): ## Not full match
                `failBreak`

        of lkAny:
          result.add quote do:
            block:
              var foundOk: bool = false
              while `posid` < `getLen`:
                if `expr`:
                  foundOk = true
                  discard `varset`

                inc `posid`

              if not foundOk:
                `failBreak`


        of lkPref:
          result.add quote do:
            while `posid` < `getLen` and `expr`:
              discard `varset`
              inc `posid`

        of lkOpt:
          var default = newLit(true)
          if elem.patt.isOptional and
             elem.bindVar.getSome(bindv) and
             elem.patt.fallback.getSome(fallback):

            default = makeVarSet(bindv, fallback, vtable, doRaise)
          result.add quote do:
            if `posid` < `getLen`:
              discard `varset`
              inc `posid`
            else:
              discard `default` ## Default

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
                discard `varset`

          result.add quote do:
            block:
              var `allOk`: bool = true
              while `posid` < `getLen` and `allOk`:
                `check`
                inc `posid`

              if not `allOk`:
                `failBreak`

        of lkTrail, lkPos:
          # ???
          discard



func makeSeqMatch(
  seqm: Match, vtable: VarTable; path: Path,
  mainExpr: string,
  doRaise: bool,
  originalMainExpr: string
     ): NimNode =
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
    getLen = newCall("len", path.toAccs(mainExpr))

  var failBreak = nnkBreakStmt.newTree(failBlock)


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

        expr = elem.patt.makeMatchExpr(
          vtable, parent, mainExpr,
          false, # WARNING no detailed reporting for subpattern
                 # matching failure. In order to get these I need to
                 # return failure string from each element, which makes
                 # thing more complicated. It is not that hard to just
                 # return `(bool, string)` or something similar, but it
                 # would require quite annoying (albeit simple)
                 # redesign of the whole code, to account for new
                 # expression types. No impossible though.

          # doRaise and (elem.kind notin {lkUntil, lkAny, lkNone})
          originalMainExpr
        )


      result.add newCommentStmtNode(
        $elem.kind & " " & elem.patt.declNode.repr)

      makeElemMatch()



  let
    comment = newCommentStmtNode(seqm.declNode.repr)
    minNode = newLit(minLen)
    maxNode = newLit(maxLen)

  var setCheck: NimNode
  if maxLen >= 5000:
    setCheck = quote do:
      `getLen` < `minNode`
  else:
    setCheck = quote do:
      `getLen` notin {`minNode` .. `maxNode`}

  if doRaise:
    let pattStr = seqm.declNode.toStrLit().codeFmt()
    let ln = seqm.declNode.lineIInfo()
    let lenObj = path.toAccs(originalMainExpr).toStrLit().codeFmt()

    if maxLen >= 5000:
      failBreak = quote do:
        {.line: `ln`.}:
          raise MatchError(
            msg: "Match failure for pattern '" & `pattStr` &
              "'. Expected at least " & $(`minNode`) &
              " elements, but " & (`lenObj`) &
              " has .len of " & $(`getLen`) & "."
          )
    else:
      failBreak = quote do:
        {.line: `ln`.}:
          raise MatchError(
            msg: "Match failure for pattern '" & `pattStr` &
              "'. Expected length in range '" & $(`minNode`) & " .. " &
              $(`maxNode`) & "', but `" & (`lenObj`) &
              "` has .len of " & $(`getLen`) & "."
          )

  result = quote do:
    `comment`
    var `matched` = false
    block `failBlock`:
      var `posid` = 0 ## Start seq match

      if `setCheck`:
        ## fail on seq len
        `failBreak`

      `result`

      `matched` = true ## Seq match ok

    `matched`

  result = result.newPar().newPar()




func makeMatchExpr(
  m: Match,
  vtable: VarTable;
  path: Path,
  mainExpr: string,
  doRaise: bool,
  originalMainExpr: string
     ): NimNode =
  case m.kind:
    of kItem:
      let parent = path.toAccs(mainExpr)
      case m.itemMatch:
        of imkInfixEq, imkSubpatt:
          if m.itemMatch == imkInfixEq:
            if m.isPlaceholder:
              result = newLit(true)
            else:
              result = nnkInfix.newTree(ident m.infix, parent, m.rhsNode)
          else:
            result = makeMatchExpr(
              m.rhsPatt, vtable, path, mainExpr, doRaise, originalMainExpr)

          if m.bindVar.getSome(vname):
            # vtable.addvar(vname, path) # XXXX
            let bindVar = makeVarSet(vname, parent, vtable, doRaise)
            if result == newLit(true):
              result = bindVar
            else:
              result = quote do:
                block:
                  if `result`:
                    `bindVar`
                  else:
                    false


        of imkPredicate:
          let pred = m.predBody
          var bindVar = newEmptyNode()
          if m.bindVar.getSome(vname):
            # vtable.addvar(vname, path) # XXXX
            bindVar = makeVarSet(vname, parent, vtable, doRaise)
          else:
            bindVar = newLit(true)

          result = quote do:
            let it {.inject.} = `parent`
            if `pred`:
              `bindVar`
            else:
              false

      if doRaise:
        let msgLit = newLit(
          "Pattern match failed: element does not match '" &
            m.declNode.toStrLit().strVal() & "'")

        result = quote do:
          `result` or ((block: raise MatchError(msg: `msgLit`) ; false))



    of kSeq:
      return makeSeqMatch(
        m, vtable, path, mainExpr, doRaise, originalMainExpr)
    of kTuple:
      var conds: seq[NimNode]
      for idx, it in m.tupleElems:
        conds.add it.makeMatchExpr(vtable, path & @[
          AccsElem(inStruct: kTuple, idx: idx)
        ], mainExpr, doRaise, originalMainExpr)

      return conds.foldInfix("and")
    of kObject:
      var conds: seq[NimNode]
      if m.kindCall.getSome(kc):
        conds.add newCall(ident "hasKind", path.toAccs(mainExpr), kc)

      for (fld, patt) in m.fldElems:
        conds.add patt.makeMatchExpr(
          vtable, path & @[
            AccsElem(inStruct: kObject, fld: fld)
          ], mainExpr, doRaise, originalMainExpr)

      if m.seqMatches.getSome(seqm):
        conds.add seqm.makeMatchExpr(
          vtable, path, mainExpr, doRaise, originalMainExpr)

      if m.kvMatches.getSome(kv):
        conds.add kv.makeMatchExpr(
          vtable, path, mainExpr, doRaise, originalMainExpr)

      return conds.foldInfix("and")

    of kPairs:
      var conds: seq[NimNode]
      for pair in m.pairElems:
        let
          accs = path.toAccs(mainExpr)
          valPath = path & @[AccsElem(
            inStruct: kPairs, key: pair.key, nocheck: m.nocheck)]

          valGet = valPath.toAccs(mainExpr)

        if m.nocheck:
          conds.add pair.patt.makeMatchExpr(
            vtable, valPath, mainExpr, doRaise, originalMainExpr)

        else:
          let
            incheck = nnkInfix.newTree(ident "in", pair.key, accs)

          if not pair.patt.isOptional:
            conds.add nnkInfix.newTree(
              ident "and", incheck,
              pair.patt.makeMatchExpr(
                vtable, valPath, mainExpr, doRaise, originalMainExpr)
            )

          else:
            let varn = pair.patt.bindVar.get
            let varsetOk = makeVarSet(varn, valGet, vtable, doRaise)
            if pair.patt.fallback.getSome(fallback):
              let varsetFail = makeVarSet(
                varn, pair.patt.fallback.get(), vtable, doRaise)

              conds.add quote do:
                block:
                  if `incheck`:
                    `varsetOk`
                  else:
                    `varsetFail`
            else:
              conds.add quote do:
                if `incheck`:
                  `varsetOk`
                else:
                  true

      return conds.foldInfix("and")
    of kAlt:
      var conds: seq[NimNode]
      for alt in m.altElems:
        conds.add alt.makeMatchExpr(
          vtable, path & @[AccsElem(inStruct: kAlt)],
          mainExpr, false, originalMainExpr)

      let res = conds.foldInfix("or")
      if not doRaise:
        return res
      else:
        let pattStr = m.declNode.toStrLit()
        return quote do:
          `res` or (block: raise MatchError(
            msg: "Match failure for pattern '" & `pattStr` &
              "' - None of the alternatives matched."
          ); true)

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


func makeMatchExpr*(
  m: Match, mainExpr: string, doRaise: bool,
  originalMainExpr: string
     ): tuple[
    expr: NimNode,
  vtable: VarTable,
  mixident: seq[string]
    ] =

  ## Create NimNode for checking whether or not item referred to by
  ## `mainExpr` matches pattern described by `Match`

  (result.vtable, result.mixident) = makeVarTable(m)
  result.expr = makeMatchExpr(
    m, result.vtable, @[],  mainExpr, doRaise, originalMainExpr)

func toNode(
  expr: NimNode,
  vtable: VarTable,
  mainExpr: string): NimNode =
  var exprNew = nnkStmtList.newTree()
  var hasOption: bool = false
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
        hasOption = true
        exprNew.add quote do:
          var `vname`: Option[typeof(`typeExpr`)]

      of vkSet, vkAlt:
        exprNew.add quote do:
          var `vname`: typeof(`typeExpr`)

      of vkRegular:
        let wasSet = ident(vname.nodeStr() & "WasSet")
        exprNew.add quote do:
          var `wasSet`: bool = false
          var `vname`: typeof(`typeExpr`)

  return quote do:
    `exprNew`
    `expr`

macro expand*(body: typed): untyped = body

macro match*(n: untyped): untyped =
  var matchcase = nnkIfStmt.newTree()
  var mixidents: seq[string]
  for elem in n[1 .. ^1]:
    case elem.kind:
      of nnkOfBranch:
        if elem[0] == ident "_":
          error("To create catch-all match use `else` clause", elem[0])

        let (expr, vtable, mixid) =
          toSeq(elem[0 .. ^2]).foldl(
            nnkInfix.newTree(ident "|", a, b)
          ).parseMatchExpr().makeMatchExpr(
            "expr", false, n.toStrLit().strVal())

        mixidents.add mixid

        matchcase.add nnkElifBranch.newTree(
          toNode(expr, vtable, "expr").newPar().newPar(),
          elem[^1]
        )

      of nnkElifBranch, nnkElse:
        matchcase.add elem
      else:
        discard

  let head = n[0]
  let pos = newCommentStmtNode($n.lineInfoObj())
  var mixinList = newStmtList nnkMixinStmt.newTree(
    mixidents.deduplicate.mapIt(
      ident it
    )
  )

  if mixidents.len == 0:
    mixinList = newEmptyNode()

  result = quote do:
    block:
      # `mixinList`
      `pos`
      let expr {.inject.} = `head`
      let pos {.inject.}: int = 0
      `matchcase`

macro assertMatch*(input, pattern: untyped): untyped =
  ## Try to match `input` using `pattern` and raise `MatchError` on
  ## failure. For DSL syntax details see start of the document.
  let
    expr = ident genSym(nskLet, "expr").repr
    (mexpr, vtable, _) = pattern.parseMatchExpr().makeMatchExpr(
      expr.repr, true, input.toStrLit().strVal())
    matched = toNode(mexpr, vtable, expr.repr)

  let patt = newLit(pattern.repr)
  result = quote do:
    let `expr` = `input`
    let ok = `matched`

macro matches*(input, pattern: untyped): untyped =
  ## Try to match `input` using `pattern` and return `false` on
  ## failure. For DSL syntax details see start of the document.
  let
    expr = ident genSym(nskLet, "expr").repr
    (mexpr, vtable, _) = pattern.parseMatchExpr().makeMatchExpr(
      expr.repr, false, input.toStrLit().strVal())
    matched = toNode(mexpr, vtable, expr.repr)

  result = quote do:
    let `expr` = `input`
    `matched`

func buildTreeMaker(
  prefix: string, resType: NimNode, match: Match): NimNode =

  case match.kind:
    of kItem:
      if (match.itemMatch == imkInfixEq):
        if match.isPlaceholder:
          if match.bindVar.getSome(bindv):
            result = newIdentNode(bindv.nodeStr())
          else:
            error(
              "Only variable placeholders allowed for pattern " &
                "construction, but expression is a `_` placeholder",
              match.declNode
            )
        else:
          if match.rhsNode != nil:
            result = match.rhsNode
          else:
            error("Empty rhs node for item", match.declNode)
      else:
        error(
          "Predicate expressions are not supported for tree " &
          "construction, use `== <expression>` to set field result"
        )
    of kObject:
      var res = newStmtList()
      let tmp = genSym(nskVar, "res")

      res.add quote do:
        var `tmp`: `resType`
        when `tmp` is ref:
          new(`tmp`)

      if match.kindCall.getSome(call):
        let kind = ident call.nodeStr().addPrefix(prefix)
        res.add quote do:
          {.push warning[CaseTransition]: off.}
          when declared(FieldDefect):
            try:
              `tmp`.kind = `kind`
            except FieldDefect:
              raise newException(FieldDefect,
                "Error while setting `kind` for " & $typeof(`tmp`) &
                  " - type does not provide `kind=` override."
              )
          else:
            `tmp`.kind = `kind`
          {.pop.}

      else:
        error(
          "Named tuple construction is not supported. To Create " &
            "object use `Kind(f1: val1, f2: val2)`" ,
          match.declNode
        )

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

func str*(node: NimNode): string = node.nodeStr()
func `str=`*(node: var NimNode, val: string) =
  if node.kind in {nnkIdent, nnkSym}:
    node = ident val
  else:
    node.strVal = val

func getTypeIdent(node: NimNode): NimNode =
  case node.getType().kind:
    of nnkObjectTy, nnkBracketExpr:
      newCall("typeof", node)
    else:
      node.getType()

macro makeTreeImpl(node, kind: typed, patt: untyped): untyped =
  var inpatt = patt
  if patt.kind in {nnkStmtList}:
    if patt.len > 1:
      inpatt = newStmtList(patt.toSeq())
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
    when not compiles((var t: T; discard t.kind)):
      static: error "No `kind` defined for " & $typeof(tmp)

    when not compiles((var t: T; t.kind = t.kind)):
      static: error "Cannot set `kind=` for " & $typeof(tmp)

    when not compiles((var t: T; t.add t)):
      static: error "No `add` defined for " & $typeof(tmp)


    makeTreeImpl(tmp, tmp.kind, patt)

template `:=`*(lhs, rhs: untyped): untyped =
  ## Shorthand for `assertMatch`
  assertMatch(rhs, lhs)

template `?=`*(lhs, rhs: untyped): untyped =
  ## Shorthand for `matches`
  matches(rhs, lhs)
