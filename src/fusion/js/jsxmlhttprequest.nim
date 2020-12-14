## `XMLHttpRequest` for the JavaScript target: https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest
when not defined(js) and not defined(nimdoc):
  {.fatal: "Module jsfetch is designed to be used with the JavaScript backend.".}

from dom import Node
export Node

type XMLHttpRequest* = ref object  ## https://xhr.spec.whatwg.org
  responseXML*: Node
  withCredentials*: bool
  status*, timeout*, readyState*: cint
  responseText*, responseURL*, statusText*: cstring

func newXMLHttpRequest*(): XMLHttpRequest {.importjs: "new XMLHttpRequest()".}
  ## Constructor for `XMLHttpRequest`.

func open*(this: XMLHttpRequest; metod, url: cstring) {.importjs: "#.open(#, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/open

func open*(this: XMLHttpRequest; metod, url: cstring; async: bool) {.importjs: "#.open(#, #, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/open

func open*(this: XMLHttpRequest; metod, url: cstring; async: bool; user: cstring) {.importjs: "#.open(#, #, #, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/open

func open*(this: XMLHttpRequest; metod, url: cstring; async: bool; user, password: cstring) {.importjs: "#.open(#, #, #, #, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/open

func send*(this: XMLHttpRequest) {.importjs: "#.send()".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/open

func send*(this: XMLHttpRequest; body: cstring) {.importjs: "#.send(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/send

func send*(this: XMLHttpRequest; body: Node) {.importjs: "#.send(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/send

func abort*(this: XMLHttpRequest) {.importjs: "#.abort()".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/abort

func getAllResponseHeaders*(this: XMLHttpRequest): cstring {.importjs: "#.getAllResponseHeaders()".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/getAllResponseHeaders

func overrideMimeType*(this: XMLHttpRequest; mimeType: cstring) {.importjs: "#.overrideMimeType(#)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/overrideMimeType

func setRequestHeader*(this: XMLHttpRequest; key, value: cstring) {.importjs: "#.setRequestHeader(#, #)".}
  ## https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/setRequestHeader

func setRequestHeader*(this: XMLHttpRequest; keyValuePairs: openArray[array[2, cstring]]) {.importjs:
  "(() => { const rqst = #; #.forEach((item) => rqst.setRequestHeader(item[0], item[1])) })()".}
  ## Same as `setRequestHeader` but takes `openArray[array[2, cstring]]`.


runnableExamples:
  if defined(nimJsXmlhttprequestTests):
    let rekuest: XMLHttpRequest = newXMLHttpRequest()
    rekuest.open("GET".cstring, "http://localhost:8000/".cstring, false)
    rekuest.setRequestHeader("mode".cstring, "no-cors".cstring)
    rekuest.setRequestHeader([["mode".cstring, "no-cors".cstring]])
    rekuest.overrideMimeType("text/plain".cstring)
    rekuest.send()
    echo rekuest.getAllResponseHeaders()
    echo "responseText\t", rekuest.responseText
    echo "responseURL\t", rekuest.responseURL
    echo "statusText\t", rekuest.statusText
    echo "responseXML\t", rekuest.responseXML is Node
    echo "status\t", rekuest.status
    echo "timeout\t", rekuest.timeout
    echo "withCredentials\t", rekuest.withCredentials
    echo "readyState\t", rekuest.readyState
    rekuest.abort()
