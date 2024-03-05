#[
autogenerated by docgen
loc: /home/runner/work/fusion/fusion/fusion/src/fusion/js/jsxmlhttprequest.nim(39, 1)
rdoccmd: 
]#
import std/assertions
import "/home/runner/work/fusion/fusion/fusion/src/fusion/js/jsxmlhttprequest.nim"
{.line: ("/home/runner/work/fusion/fusion/fusion/src/fusion/js/jsxmlhttprequest.nim", 39, 1).}:
  from std/dom import Node
  if defined(fusionJsXmlhttprequestTests):
    let request: XMLHttpRequest = newXMLHttpRequest()
    request.open("GET".cstring, "http://localhost:8000/".cstring, false)
    request.setRequestHeader("mode".cstring, "no-cors".cstring)
    request.setRequestHeader([(key: "mode".cstring, val: "no-cors".cstring)])
    request.overrideMimeType("text/plain".cstring)
    request.send()
    echo request.getAllResponseHeaders()
    echo "responseText\t", request.responseText
    echo "responseURL\t", request.responseURL
    echo "statusText\t", request.statusText
    echo "responseXML\t", request.responseXML is Node
    echo "status\t", request.status
    echo "timeout\t", request.timeout
    echo "withCredentials\t", request.withCredentials
    echo "readyState\t", request.readyState
    request.abort()

