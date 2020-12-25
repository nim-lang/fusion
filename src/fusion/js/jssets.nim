## `Set` for the JavaScript target.
## * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set
when not defined(js) and not defined(nimdoc):
  {.fatal: "Module jssets is designed to be used with the JavaScript backend.".}

type JsSet* = ref object of JsRoot ## Set API.

func newJsSet*(): JsSet {.importjs: "new Set()".} ## Constructor for `JsSet`.

func newJsSet*(items: openArray[SomeNumber]): JsSet {.importjs: "new Set(#)".} ## Constructor for `JsSet`.

func add*(this: JsSet; value: SomeNumber) {.importjs: "#.$1(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/add

func delete*(this: JsSet; value: SomeNumber) {.importjs: "#.$1(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/delete

func clear*(this: JsSet) {.importjs: "#.$1()".}
  ## https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/clear

func contains*(this: JsSet; value: SomeNumber): bool {.importjs: "#.has(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/has

func getInt*(this: JsSet): seq[int] {.importjs: """
  (() => {const x = []; #.forEach(a => x.push(parseInt(a))); return x})()""".}
  ## Convert `JsSet` to `seq[int]`, all items will be converted to `int`.

func getFloat*(this: JsSet): seq[float] {.importjs: """
  (() => {const x = []; #.forEach(a => x.push(parseFloat(a))); return x})()""".}
  ## Convert `JsSet` to `seq[float]`, all items will be converted to `float`.


runnableExamples:
  if defined(fusionJsSetTests):
    block:
      let a: JsSet = newJsSet([1, 2, 3, 4, 5])
      let b: JsSet = newJsSet([1.0, 2.0, 3.0])
      doAssert a.getInt() == @[1, 2, 3, 4, 5]
      doAssert b.getFloat() == @[1.0, 2.0, 3.0]
      a.clear()
      b.clear()
      doAssert a.getInt() == @[]
      doAssert b.getFloat() == @[]
      let d: JsSet = newJsSet([1, 2, 3])
      d.add(4)
      d.delete(2)
      doAssert 3 in d
      doAssert d.contains 1

    block:
      let a = newJsSet([1, 2])
      a.add(3.5)
      doAssert a.getInt() == @[1, 2, 3]

      let b = newJsSet([1.0, 2.0])
      b.add(3)
      doAssert b.getFloat() == @[1.0, 2.0, 3.0]
