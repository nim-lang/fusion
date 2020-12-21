## - HTTP Headers for the JavaScript target: https://developer.mozilla.org/en-US/docs/Web/API/Headers
when not defined(js) and not defined(nimdoc):
  {.fatal: "Module jsheaders is designed to be used with the JavaScript backend.".}

type Headers* = ref object of JsRoot ## HTTP Headers for the JavaScript target.

func newHeaders*(): Headers {.importjs: "new Headers()".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/Headers

func append*(this: Headers; key: cstring; value: cstring) {.importjs: "#.$1(#, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/Headers/append

func delete*(this: Headers; key: cstring) {.importjs: "#.$1(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/Headers/delete

func has*(this: Headers; key: cstring): bool {.importjs: "#.$1(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/Headers/has

func keys*(this: Headers): seq[cstring] {.importjs: "Array.from(#.$1())".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/Headers/keys

func values*(this: Headers): seq[cstring] {.importjs: "Array.from(#.$1())".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/Headers/values

func entries*(this: Headers): seq[array[2, cstring]] {.importjs: "Array.from(#.$1())".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/Headers/entries

func `[]`*(this: Headers; key: cstring): cstring {.importjs: "#.get(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/Headers/get

func `[]=`*(this: Headers; key: cstring; value: cstring) {.importjs: "#.set(#, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/Headers/set

func clear*(this: Headers) {.importjs:
  "(() => { const header = #; Array.from(header.keys()).forEach((key) => header.delete(key)) })()".}
  ## Convenience func to delete all items from `Headers`.


runnableExamples:
  if defined(fusionJsHeadersTests):
    let header = newHeaders()
    header.append(r"key", r"value")
    doAssert header.has(r"key")
    doAssert header.keys() == @["key".cstring]
    doAssert header.values() == @["value".cstring]
    doAssert header[r"key"] == "value".cstring
    header[r"other"] = r"another"
    doAssert header[r"other"] == "another".cstring
    doAssert header.entries() == @[["key".cstring, "value"], ["other".cstring, "another"]]
    header.delete(r"other")
    doAssert header.entries() == @[]
