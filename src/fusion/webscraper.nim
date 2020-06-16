## Web Scraper
## ===========
##
## * `webscraper` can help you implement a *simple basic* web scraper with standard library.
## `webscraper` builds on top of `htmlparser`, `xmltree`, `strtabs`, `strutils`.
## Feed it the HTML `body` using `httpclient` and find tags, attributes, values, etc.
## If you need a `seq` as output, see `sugar.collect` or `sequtils.toSeq`. Since: 1.3
##
## .. code-block:: nim
##   let body: string = newHttpClient().getContent("https://example.io/data.html")
##   for item in scrap(body, tag = "textarea", attr = "", pred = nimbleModule.filterData):
##     var myDataset = nimbleModule.processData(item) # Pseudocode example.
##     # customModule.someFunctionToProcessData(item)
##
## Future directions: Better filtering capabilities for the HTML.
##
## See also:
## * `httpclient <https://nim-lang.org/docs/httpclient.html>`_

import htmlparser, xmltree, strtabs, strutils, algorithm, std/private/since
export xmltree, strtabs, strutils

proc parseFindImpl(body, tag: string; reversedIter: bool): seq[XmlNode] {.inline.} =
  result = xmltree.findAll(htmlparser.parseHtml(body), tag, true)
  if reversedIter: reverse(result)

template hasAttrImpl(node: XmlNode; attr: string): bool =
  attr.len == 0 or (node.attrs != nil and node.attrs.hasKey(attr) and node.attr(attr).len > 0)

iterator scrap*(body, tag, attr, attrValue: string; reversedIter = false): XmlNode {.since: (1, 3).} =
  assert tag.len > 0, "tag must not be empty string"
  for n in parseFindImpl(body, tag, reversedIter):
    if hasAttrImpl(n, attr) and contains(n.attr(attr).toLowerAscii, attrValue): yield n

iterator scrap*(body, tag, attr: string; pred: proc (n: XmlNode): bool; reversedIter = false): XmlNode {.since: (1, 3).} =
  assert tag.len > 0, "tag must not be empty string"
  for n in parseFindImpl(body, tag, reversedIter):
    if hasAttrImpl(n, attr) and pred(n): yield n

iterator scrap*(body: string; tag: string; reversedIter = false): XmlNode {.since: (1, 3).} =
  assert tag.len > 0, "tag must not be empty string"
  for n in parseFindImpl(body, tag, false): yield n

runnableExamples:
  static:
    const body = """
    <body><script>console.log(42)</script>
      <h1>This is a test HTML Sample</h1>
      <p id="owo" key="value"> text </p>
      <p class="owo" id="value"> text </p>
      <hr><br><style>*{color:red}</style>
    </body><!-- Compile-Time Web Scraper -->
    """  ## Just an example html body string.
    func example(n: XmlNode): bool = """ key="value" """ in $n ## Just an example proc

    ## Scrap data by searching by HTML Tag.
    for item in scrap(body = body, tag = "p"):
      doAssert $item == """<p key="value" id="owo"> text </p>"""
      break

    ## Scrap data by searching by HTML Tag in reverse from bottom to top of page.
    for item in scrap(body = body, tag = "p", reversedIter = true):
      doAssert $item == """<p key="value" id="owo"> text </p>"""
      break

    ## Scrap data by searching by HTML tag, attribute key and a predicate function.
    for item in scrap(body = body, tag = "p", attr = "key", pred = example):
      doAssert $item == """<p key="value" id="owo"> text </p>"""

    ## Scrap data by searching by HTML tag and a predicate function.
    for item in scrap(body = body, tag = "p", attr = "", pred = example):
      doAssert $item == """<p key="value" id="owo"> text </p>"""

    ## Scrap data by searching by HTML Tag, attribute key and attribute value.
    for item in scrap(body = body, tag = "p", attr = "key", attrValue = "value"):
      doAssert $item == """<p key="value" id="owo"> text </p>"""
