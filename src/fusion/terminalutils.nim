## * Utilities and convenience procs built on top of `terminal` module.
##
## License
## =======
##
## `promptInteractive` code is based and adapted from Nimble source code to Nim stdlib,
## Nimble is distributed with Nim, Nimble itself is under the following license:
##
## Copyright (c) 2015, Dominik Picheta
## All rights reserved.
##
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are met:
## 1. Redistributions of source code must retain the above copyright
##    notice, this list of conditions and the following disclaimer.
## 2. Redistributions in binary form must reproduce the above copyright
##    notice, this list of conditions and the following disclaimer in the
##    documentation and/or other materials provided with the distribution.
## 3. Neither the name of Nimble nor the
##    names of its contributors may be used to endorse or promote products
##    derived from this software without specific prior written permission.
##
## THIS SOFTWARE IS PROVIDED BY DOMINIK PICHETA ''AS IS'' AND ANY
## EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
## WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
## DISCLAIMED. IN NO EVENT SHALL DOMINIK PICHETA BE LIABLE FOR ANY
## DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
## (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
## LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
## ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
## SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##
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
