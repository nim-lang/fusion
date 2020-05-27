import packages/docutils/rstgen, packages/docutils/rst
export rstgen, rst

proc rstToLatex*(rstSource: string, options: RstParseOptions): string {.inline.} =
  ## Convenience proc for `renderRstToOut` and `initRstGenerator`.
  runnableExamples: doAssert rstToLatex("*Hello* **world**", {}) == """\emph{Hello} \textbf{world}"""
  if rstSource.len == 0: return
  var option: bool
  var rstGenera: RstGenerator
  rstGenera.initRstGenerator(outLatex, defaultConfig(), "input", options)
  rstGenera.renderRstToOut(rstParse(rstSource, "", 1, 1, option, options), result)
