## - `FormData` for the JavaScript target: https://developer.mozilla.org/en-US/docs/Web/API/FormData
when not defined(js) and not defined(nimdoc):
  {.fatal: "Module jsformdata is designed to be used with the JavaScript backend.".}

type FormData* = ref object of JsRoot ## FormData API.

func newFormData*(): FormData {.importjs: "new FormData()".}

func append*(this: FormData; name, value: cstring) {.importjs: "#.append(#, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/append

func append*(this: FormData; name: cstring; value: bool) {.importjs: "#.append(#, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/append

func append*(this: FormData; name: cstring; value: SomeNumber) {.importjs: "#.append(#, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/append

func append*(this: FormData; name, value, filename: cstring) {.importjs: "#.append(#, #, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/append

func append*(this: FormData; name: cstring; value: bool; filename: cstring) {.importjs: "#.append(#, #, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/append

func append*(this: FormData; name: cstring; value: SomeNumber; filename: cstring) {.importjs: "#.append(#, #, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/append

func delete*(this: FormData; name: cstring) {.importjs: "#.delete(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/delete

func get*(this: FormData; name: cstring): cstring {.importjs: "#.get(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/get

func getAll*(this: FormData; name: cstring): seq[cstring] {.importjs: "#.getAll(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/getAll

func has*(this: FormData; name: cstring): bool {.importjs: "#.has(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/has

func set*(this: FormData; name, value: cstring; filename = "".cstring) {.importjs: "#.set(#, #, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/set

func keys*(this: FormData): seq[cstring] {.importjs: "Array.from(#.keys())".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/keys

func values*(this: FormData): seq[cstring] {.importjs: "Array.from(#.values())".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/values

func entries*(this: FormData): seq[array[2, cstring]] {.importjs: "Array.from(#.entries())".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/FormData/entries

func clear*(this: FormData) {.importjs:
  "(() => { const frmdt = #; Array.from(frmdt.keys()).forEach((key) => frmdt.delete(key)) })()".}
  ## Convenience func to delete all items from `FormData`.


runnableExamples:
  if defined(fusionJsFormdataTests):
    let data: FormData = newFormData()
    data.set("key0".cstring, "value0".cstring)
    data.append("key1".cstring, "value1".cstring)
    data.delete("key1".cstring)
    doAssert data.has("key0".cstring)
    doAssert data.get("key0".cstring) == "value0".cstring
