## Web Scraper
## ===========
##
## * `webscraper` can help you implement a *simple basic* web scraper with standard library.
## `webscraper` builds on top of `htmlparser`, `xmltree`, `strtabs`, `strutils`, `pegs`.
## Feed it the HTML DOM using `httpclient.getContent` and find tags, attributes, values, etc.
## If you need a `seq` as output, see `sugar.collect` or `sequtils.toSeq`.
##
## **See also:**
## * `httpclient <https://nim-lang.org/docs/httpclient.html>`_

import xmltree, strtabs, strutils, algorithm, pegs
export xmltree, strtabs

func match(n: XmlNode, s: tuple[id: string, tag: string, combi: char, class: seq[string]]): bool =
  result = (s.tag.len == 0 or s.tag == "*" or s.tag == n.tag)
  if result and s.id.len > 0: result = s.id == n.attr"id"
  if result and s.class.len > 0:
    for class in s.class: result = n.attr("class").len > 0 and class in n.attr("class").split

func find(parent: XmlNode, selector: tuple[id: string, tag: string, combi: char, class: seq[string]], found: var seq[XmlNode]) =
  for child in parent.items:
    if child.kind == xnElement:
      if match(child, selector): found.add(child)
      if selector.combi != '>': child.find(selector, found)

proc find(parents: var seq[XmlNode], selector: tuple[id: string, tag: string, combi: char, class: seq[string]]) =
  var found: seq[XmlNode]
  for p in parents: find(p, selector, found)
  parents = found

proc multiFind(parent: XmlNode, selectors: seq[tuple[id: string, tag: string, combi: char, class: seq[string]]], found: var seq[XmlNode]) =
  var matches: seq[int]
  var start: seq[int]
  start = @[0]
  for i in 0 ..< selectors.len:
    var selector = selectors[i]
    matches = @[]
    for j in start:
      for k in j ..< parent.len:
        var child = parent[k]
        if child.kind == xnElement and match(child, selector):
          if i < selectors.len - 1: matches.add(k + 1)
          else:
            if not found.contains(child): found.add(child)
          if selector.combi == '+': break
    start = matches

proc multiFind(parents: var seq[XmlNode], selectors: seq[tuple[id: string, tag: string, combi: char, class: seq[string]]]) =
  var found: seq[XmlNode]
  for p in parents: multiFind(p, selectors, found)
  parents = found

proc parseSelector(token: string): tuple[id: string, tag: string, combi: char, class: seq[string]] =
  result = (id: "", tag: "", combi: ' ', class: @[])
  if token == "*": result.tag = "*"
  elif token =~ peg"""\s*{\ident}?({'#'\ident})? ({\.[a-zA-Z0-9_][a-zA-Z0-9_\-]*})* {\[[a-zA-Z][a-zA-Z0-9_\-]*\s*([\*\^\$\~]?\=\s*[\'""]?(\s*\ident\s*)+[\'""]?)?\]}*""":
    for i in 0 ..< matches.len:
      if matches[i].len == 0: continue
      case matches[i][0]:
      of '#': result.id = matches[i][1..^1]
      of '.': result.class.add(matches[i][1..^1])
      else: result.tag = matches[i]

proc findCssImpl(node: seq[XmlNode], cssSelector: string): seq[XmlNode] {.noinline.} =
  assert cssSelector.len > 0, "cssSelector must not be empty string"
  result = node
  var tokens = cssSelector.strip.split
  for pos in 0 ..< tokens.len:
    var isSimple = true
    if pos > 0 and (tokens[pos - 1] == "+" or tokens[pos - 1] == "~"): continue
    if tokens[pos] in [">", "~", "+"]: continue
    var selector = parseSelector(tokens[pos])
    if pos > 0 and tokens[pos-1] == ">": selector.combi = '>'
    var selectors = @[selector]
    var i = 1
    while true:
      if pos + i >= tokens.len: break
      var nextCombi = tokens[pos + i]
      if nextCombi == "+" or nextCombi == "~":
        if pos + i + 1 >= tokens.len: assert false, "Selector not found"
      else: break
      isSimple = false
      var nextToken = tokens[pos + i + 1]
      inc i, 2
      var temp = parseSelector(nextToken)
      temp.combi = nextCombi[0]
      selectors.add(temp)
    if isSimple: result.find(selectors[0]) else: result.multiFind(selectors)

func parseFindImpl(body: XmlNode; tag: string; reversedIter: bool): seq[XmlNode] {.inline.} =
  assert tag.len > 0, "tag must not be empty string"
  result = xmltree.findAll(body, tag, true)
  if reversedIter: reverse(result)

template hasAttrImpl(node: XmlNode; attr: string): bool =
  attr.len == 0 or (node.attrs != nil and node.attrs.hasKey(attr) and node.attr(attr).len > 0)

iterator scrap*(body: XmlNode; tag: string; attr: string; attrValue: string; reversedIter = false): XmlNode =
  ## Web scraper iterator that searchs by tag, attribute, attribute value.
  ##
  ## * `body` is the HTML DOM `XmlNode`, feed it with `htmlclient.getContent` and `htmlparser.parseHtml`.
  ## * `tag` is the HTML DOM tag, must not be empty string, for example to search for `<a>` links use `"a"`, etc.
  ## * `attr` is an *attribute key* that the `tag` should match, if you do *not* want to filter by Attribute key then use empty string.
  ## * `attrValue` is an *attribute value* that the `attr` should match, can be empty string.
  ## * `reversedIter` Reverses the iteration order, set to `true` to scan from bottom to top of the page.
  for n in parseFindImpl(body, tag, reversedIter):
    if hasAttrImpl(n, attr) and contains(n.attr(attr).toLowerAscii, attrValue): yield n

iterator scrap*(body: XmlNode; tag: string; attr: string; pred: proc (n: XmlNode): bool; reversedIter = false): XmlNode =
  ## Web scraper iterator that searchs by tag, attribute and a user-provided arbitrary filtering function.
  ##
  ## * `body` is the HTML DOM `XmlNode`, feed it with `htmlclient.getContent` and `htmlparser.parseHtml`.
  ## * `tag` is the HTML DOM tag, must not be empty string, for example to search for `<a>` links use `"a"`, etc.
  ## * `attr` is an *attribute key* that the `tag` should match, if you do *not* want to filter by attribute key then use empty string.
  ## * `pred` is a user-provided arbitrary filtering function that takes `XmlNode` and returns `bool`.
  ## * `reversedIter` Reverses the iteration order, set to `true` to scan from bottom to top of the page.
  for n in parseFindImpl(body, tag, reversedIter):
    if hasAttrImpl(n, attr) and pred(n): yield n

iterator scrap*(body: XmlNode; tag: string; cssSelector: string; reversedIter = false): XmlNode =
  ## Web scraper iterator that searchs by tag and CSS Selector.
  ##
  ## * `body` is the HTML DOM `XmlNode`, feed it with `htmlclient.getContent` and `htmlparser.parseHtml`.
  ## * `tag` is the HTML DOM tag, must not be empty string, for example to search for `<a>` links use `"a"`, etc.
  ## * `cssSelector` is a CSS selector to filter by, must not be empty string nor Regex syntax.
  ## * `reversedIter` Reverses the iteration order, set to `true` to scan from bottom to top of the page.
  assert cssSelector.len > 0, "cssSelector must not be empty string"
  for n in findCssImpl(parseFindImpl(body, tag, reversedIter), cssSelector): yield n


runnableExamples:
  import htmlparser
  static:
    block:
      const htmls = """
      <body><script>console.log(42)</script>
        <h1>This is a test HTML Sample</h1>
        <p id="owo" key="value"> text </p>
        <p class="owo" id="value"> text </p>
        <hr><br><style>*{color:red}</style>
      </body><!-- Compile-Time Web Scraper -->
      """  ## Just an example html body string.
      let body: XmlNode = htmlparser.parseHtml(htmls)
      func example(n: XmlNode): bool = """ key="value" """ in $n ## Just an example proc

      ## Scrap data by searching by HTML tag, attribute key and a predicate function.
      for item in scrap(body = body, tag = "p", attr = "key", pred = example):
        doAssert $item == """<p key="value" id="owo"> text </p>"""

      ## Scrap data by searching by HTML tag and a predicate function.
      for item in scrap(body = body, tag = "p", attr = "", pred = example):
        doAssert $item == """<p key="value" id="owo"> text </p>"""

      ## Scrap data by searching by HTML Tag, attribute key and attribute value.
      for item in scrap(body = body, tag = "p", attr = "key", attrValue = "value"):
        doAssert $item == """<p key="value" id="owo"> text </p>"""

    block:
      const htmls = """
      <html>
      <head>
        <title>Title</title>
      </head>
      <body>
        <nav>
          <p>OwO</p>
          <ul class="menu">
            <li>
              <a href="#">Linky</a>
            </li>
          </ul>
        </nav>
        <form>
          <div>
            <input type="email" class="mail"/>
          </div>
          <div>
            <button id="stuff" type="submit"></button>
          </div>
        </form>
      </body>
      </html> """  ## Just an example HTML.
      let body: XmlNode = htmlparser.parseHtml(htmls)

      ## Scrap data by searching by CSS Selector value (No Regex).
      for item in scrap(body = body, tag = "body", cssSelector = "nav p"):
        doAssert $item == """<p>OwO</p>"""

      ## Uses "tag" because most of the time you want to get data from <body>,
      ## this allows to quickly ignore <head>, <template>, <script>, etc.
      for item in scrap(body = body, tag = "body", cssSelector = "head *"):
        doAssert $item == """<title>Title</title>"""

      for item in scrap(body = body, tag = "body", cssSelector = "body nav ul.menu > li > a"):
        doAssert $item == """<a href="#">Linky</a>"""

      for item in scrap(body = body, tag = "body", cssSelector = "body form div input.mail"):
        doAssert $item == """<input type="email" class="mail" />"""

      for item in scrap(body = body, tag = "body", cssSelector = "body form div button#stuff"):
        doAssert $item == """<button type="submit" id="stuff" />"""

      for item in scrap(body = body, tag = "body", cssSelector = "#stuff"):
        doAssert $item == """<button type="submit" id="stuff" />"""
