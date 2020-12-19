## - `XMLSerializer` for the JavaScript target: https://developer.mozilla.org/en-US/docs/Web/API/XMLSerializer
when not defined(js) and not defined(Nimdoc):
  {.fatal: "Module jsxmlserializer is designed to be used with the JavaScript backend.".}

from dom import Node
export Node

type XMLSerializer* = ref object of JsRoot  ## XMLSerializer API.

func newXMLSerializer*(): XMLSerializer {.importjs: "new XMLSerializer()".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLSerializer

func serializeToString*(this: XMLSerializer; node: Node): cstring {.importjs: "#.serializeToString(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLSerializer/serializeToString

func serializeToString*(node: Node): cstring {.importjs:
  "(() => { const srlzr = new XMLSerializer(); return srlzr.serializeToString(#) })()".}
  ## Convenience func for `XMLSerializer` that returns `cstring` directly.


runnableExamples:
  from dom import document
  if defined(fusionJsXMLSerializerTests):
    let cerealizer: XMLSerializer = newXMLSerializer()
    echo cerealizer.serializeToString(node = document)
