## `Set` for the JavaScript target.
## * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set
when not defined(js) and not defined(nimdoc):
  {.fatal: "Module jsset is designed to be used with the JavaScript backend.".}

type JSSet* = ref object of JsRoot ## Set API.

func newJSSet*(): JSSet {.importjs: "new Set()".} ## Constructor for `JSSet`.

func newJSSet*(items: openArray[SomeNumber]): JSSet {.importjs: "new Set(#)".} ## Constructor for `JSSet`.

func add*(this: JSSet; value: SomeNumber) {.importjs: "#.$1(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/add

func delete*(this: JSSet; value: SomeNumber) {.importjs: "#.$1(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/delete

func clear*(this: JSSet) {.importjs: "#.$1()".}
  ## https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/clear

func contains*(this: JSSet; value: SomeNumber): bool {.importjs: "#.has(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Set/has

func getInt*(this: JSSet): seq[int] {.importjs: """
  (() => {const x = []; #.forEach(a => x.push(parseInt(a))); return x})()""".}
  ## Convert `JSSet` to `seq[int]`, all items will be converted to `int`.

func getFloat*(this: JSSet): seq[float] {.importjs: """
  (() => {const x = []; #.forEach(a => x.push(parseFloat(a))); return x})()""".}
  ## Convert `JSSet` to `seq[float]`, all items will be converted to `float`.


runnableExamples:
  if defined(fusionJsSetTests):
    block:
      let a: JSSet = newJSSet([1, 2, 3, 4, 5])
      let b: JSSet = newJSSet([1.0, 2.0, 3.0])
      doAssert a.getInt() == @[1, 2, 3, 4, 5]
      doAssert b.getFloat() == @[1.0, 2.0, 3.0]
      a.clear()
      b.clear()
      doAssert a.getInt() == @[]
      doAssert b.getFloat() == @[]
      let d: JSSet = newJSSet([1, 2, 3])
      d.add(4)
      d.delete(2)
      doAssert 3 in d
      doAssert d.contains 1

    block:
      let a = newJSSet([1, 2])
      a.add(3.5)
      doAssert a.getInt() == @[1, 2, 3]

      let b = newJSSet([1.0, 2.0])
      b.add(3)
      doAssert b.getFloat() == @[1.0, 2.0, 3.0]
