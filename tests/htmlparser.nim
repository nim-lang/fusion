import unittest,os,strutils

import fusion/htmlparser
import fusion/htmlparser/xmltree
test "test demo.html":
  let expected = readFile("tests"  / "demo.expect.html")
  let html = parseHtml(readFile("tests"  / "demo.html") )
  var cleanHtml = ($(html.child("html")))
  cleanHtml.stripLineEnd
  var cleanExpected = ($expected)
  cleanExpected.stripLineEnd
  check cleanHtml == cleanExpected