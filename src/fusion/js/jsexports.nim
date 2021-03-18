when not defined(js):
  {.fatal: "Module jsexports is designed to be used with the JavaScript backend.".}

import macros

macro jsExports*(body: untyped) =
  ## Export statement for JavaScript targets.
  ## * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/export
  ## * https://caniuse.com/mdn-javascript_statements_export
  runnableExamples("-r:off -b:js"):
    let
      example0 = "example".cstring
      example1 = 42.cint
      example2 = 3.14
      example3 = true
      `foo  bar` = "Stropping Example".cstring

    jsExports:
      example0             ## Simple export, same name.
      "default" = example1 ## Export with a different name.
      (example2, example3) ## Export multiple symbols at once.
      foobar

  var exports = @[newStrLitNode"export { "]

  template exportStatement(node: NimNode) =
    var unmangled, mangled: NimNode
    if node.kind == nnkAsgn:  # alias = symbol
      unmangled = node[0]
      mangled = node[1]
    elif node.kind == nnkIdent:
      unmangled = node.toStrLit
      mangled = node
    exports.add mangled
    exports.add newStrLitNode" as "
    exports.add unmangled
    exports.add newStrLitNode", "

  if body.len == 0:
    exportStatement(body)
  for node in body:
    if node.len > 0:
      if node.kind == nnkAsgn:
        exportStatement(node)
      elif node.kind == nnkPar:
        for child in node:
          exportStatement(child)
    else:
      exportStatement(node)
  exports.add(newStrLitNode"};")
  quote do:
    {.emit: `exports` .}


macro jsImports*(fromModule: static[string]; body: untyped) =
  ## Import statement for JavaScript targets.
  ## `fromModule` argument is the origin JavaScript module.
  ## * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/import
  ## * https://caniuse.com/mdn-javascript_statements_import
  runnableExamples("-r:off -b:js"):
    let
      example0 {.importc, nodecl.}: int
      example1 {.importc, nodecl.}: bool
      example2 {.importc, nodecl.}: float
      example3 {.importc, nodecl.}: cint
      `foo  bar` {.importc, nodecl.}: cstring

    jsImports("somelib.js"):
      example0             ## Simple import, same name.
      "default" = example1 ## Import with a different name.
      (example2, example3) ## Import multiple symbols at once.
      foobar

  doAssert fromModule.len > 0, "fromModule must not be empty string"
  var imports = @[newStrLitNode"import { "]

  template importStatement(node: NimNode) =
    var unmangled, mangled: NimNode
    if node.kind == nnkAsgn:  # alias = symbol
      unmangled = node[0]
      mangled = node[1].toStrLit
      imports.add mangled
      imports.add newStrLitNode" as "
      imports.add unmangled
    elif node.kind == nnkIdent:
      mangled = node.toStrLit
      imports.add mangled
    imports.add newStrLitNode", "

  if body.len == 0:
    importStatement(body)
  for node in body:
    if node.len > 0:
      if node.kind == nnkAsgn:
        importStatement(node)
      elif node.kind == nnkPar:
        for child in node:
          importStatement(child)
    else:
      importStatement(node)
  imports.add(newStrLitNode"} from ")
  imports.add(newLit(fromModule).toStrLit)
  imports.add(newStrLitNode";")
  quote do:
    {.emit: `imports` .}
