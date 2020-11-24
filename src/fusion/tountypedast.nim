import macros

proc toUntypedAst*(n: NimNode): NimNode =
  ## Replaces every `nnkSym` node in `n` by a fresh identifier node.
  ## This forces the compiler to perform a new lookup pass.
  runnableExamples:
    macro lc(init: untyped): untyped =
      expectKind(init, {nnkIdent, nnkSym})
      let x = toUntypedAst(init)
      expectKind(x, nnkIdent)
      result = newStmtList()

    newSeq.lc()
    lc(newSeq)

  case n.kind:
  of nnkSym:
    result = newIdentNode(n.repr)
  of nnkNone, nnkEmpty, nnkIdent, nnkLiterals:
    result = n
  of nnkClosedSymChoice, nnkOpenSymChoice:
    result = toUntypedAst(n[0])
  else:
    result = copyNimNode(n)
    for x in n:
      result.add toUntypedAst(x)
