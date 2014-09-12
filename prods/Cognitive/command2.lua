praxis:

setBufferText("")
clearError()
clearTrace()

-- directcomposition.lua

fugue.lines[1] = {}
fugue.currnote = 1
fugue.beatDur = 200
table.insert(fugue.lines[1],
  { degree = 7, octave = -1, duration = fugue.beatDur * 0.5 })
table.insert(fugue.lines[1],
  { degree = 1, octave = 0, duration = fugue.beatDur * 1 })
table.insert(fugue.lines[1],
  { degree = 2, octave = 0, duration = fugue.beatDur * 0.5 })
table.insert(fugue.lines[1],
  { degree = 3, octave = 0, duration = fugue.beatDur * 1 })
table.insert(fugue.lines[1],
  { degree = 4, octave = 0, duration = fugue.beatDur * 0.5 })
table.insert(fugue.lines[1],
  { degree = 5, octave = 0, duration = fugue.beatDur * 1 })
fugue.play()

