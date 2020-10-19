## * Utilities and convenience procs built on top of `terminal` module.
##
## See also:
## * `terminal <terminal.html>`_
import terminal


proc promptInteractive*(question: string; answers: openArray[string]; width: Positive; indicators = ['>', '<']): string =
  ## Terminal prompt that asks a `question` and returns only one of the answers from possible `answers`.
  ##
  ## .. code-block:: Nim
  ##   echo promptInteractive("Is Schrödinger's Cat alive?", ["yes", "no", "maybe"], 40, ['>', '-'])
  ##
  # Adapted from Nimble source code to stdlib, adding width argument.
  assert question.len > 0, "Question must not be empty"
  assert answers.len > 0, "There must be at least one possible answer"
  writeStyled(question, {styleBright})
  var
    current = 0
    selected = false
  # Incase the cursor is at the bottom of the terminal
  for arg in answers: stdout.write "\n"
  # Reset the cursor to the start of the selection prompt
  cursorUp(stdout, answers.len)
  cursorForward(stdout, width)
  hideCursor(stdout)

  # The selection loop
  while not selected:
    setForegroundColor(fgDefault)
    # Loop through the options
    for i, arg in answers:
      # Check if the option is the current
      if i == current:
        writeStyled($indicators[0] & " " & arg & " " & $indicators[1], {styleBright, styleUnderscore})
      else:
        writeStyled("  " & arg & "  ", {styleDim})
      # Move the cursor back to the start
      stdout.cursorBackward(arg.len + 4)
      # Move down for the next item
      cursorDown(stdout)
    # Move the cursor back up to the start of the selection prompt
    stdout.cursorUp(answers.len)
    resetAttributes(stdout)

    # Ensure that the screen is updated before input
    flushFile(stdout)
    # Begin key input
    while true:
      case getch():
      of '\t', '\x1B':
        current = (current + 1) mod answers.len
        break
      of '\r', ' ':
        selected = true
        break
      of '\3':
        showCursor(stdout)
        raise newException(OSError, "Keyboard interrupt")
      else: discard

  # Erase all lines of the selection
  for i in 0 ..< answers.len:
    eraseLine(stdout)
    cursorDown(stdout)
  # Move the cursor back up to the initial selection line
  stdout.cursorUp(answers.len)
  showCursor(stdout)
  result = answers[current]
