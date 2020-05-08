from os import commandLineParams

proc getOpt*(key: static[string]; parseProc: proc; default: any;
    cmdline: seq[TaintedString] = os.commandLineParams();
    shortOpts: static[bool] = false, prefix = '-', seps = {':', '='}): auto {.inline.} =
  ## Fast simple `parseopt` alternative, parse anything, returns concrete type value directly.
  ## This is a convenience proc.
  ##
  ## * `key` is the Key to parse from `os.commandLineParams()`, must not be empty string.
  ## * `parseProc` is whatever `proc` parses the value of `key`, any `proc` should work.
  ## * `default` is a default value to return if `key` is not found, `any` type value should work.
  ## * `shortOpts` if `true` then `-key=value` short format is allowed too (slower).
  ## * `prefix` is 1 `char` for prefix for key, `prefix = '+'` then `++key=value` is parsed.
  ## * `seps` is 1 `set[char]` for separator of `key` and value, `seps={'@'}` then `--key@value` is parsed.
  ##
  ## Examples:
  ##
  ## .. code-block:: nim
  ##   import strutils, json                                  ## Imports just for the example.
  ##   echo getOpt("foo", parseInt, 0)                        ## --foo=42
  ##   echo getOpt("bar", parseBool, false)                   ## --bar:true
  ##   echo getOpt("baz", parseHexStr, "f0f0")                ## --baz:bebe
  ##   echo getOpt("bax", readFile, "default value")          ## --bax:file.ext
  ##   echo getOpt("bay", json.parseFile, %*{"key": "value"}) ## --bay:data.json
  ##   echo getOpt("?", parseOctInt, 0o666)                   ## --?=0o777
  ##   echo getOpt("x", parseBinInt, 1010, shortOpts = true)  ## -x=1111
  ##   echo getOpt("owo", parseUInt, 9.uint, shortOpts=true, prefix='+', seps={'@'}) ## +owo@42
  ##
  ## Works with any arbitrary custom types and custom parse procs too:
  ##
  ## .. code-block:: nim
  ##   type Custom = distinct string               ## Just an example Type.
  ##   func example(a: string): Custom = Custom(a) ## Just an example parse proc.
  ##   doAssert getOpt("key", example, Custom("")) is Custom
  ##
  ## You can enforce required mandatory command line params by using `options`:
  ##
  ## .. code-block:: nim
  ##   func example(x: string): Option[int] = some(parseInt(x)) ## Just an example parse proc.
  ##   doAssert getOpt("required", example, none(int)).isSome   ## --required=9
  ##
  ## You can enforce required mandatory command line params by checking against the `default` value:
  ##
  ## .. code-block:: nim
  ##   doAssert getOpt("required", parseInt, 0) != 0  ## --required=9
  ##
  ## See also:
  ## * `parseopt <https://nim-lang.org/docs/parseopt.html>`_
  ## * `options <https://nim-lang.org/docs/options.html>`_
  assert key.len > 0, "Key must not be empty string"
  assert prefix != ' ' and ' ' notin seps, "prefix and seps must not be empty char"
  result = default
  for x in cmdline:
    if x[0] == prefix and x[1] == prefix and x[static(key.len + 2)] in seps:
      if x[static(2..key.len + 1)] == key: return parseProc(x[static(key.len + 3..^1)])
    when shortOpts:
      if x[0] == prefix and x[static(key.len + 1)] in seps:
        if x[static(1..key.len)] == key: return parseProc(x[static(key.len + 2..^1)])
