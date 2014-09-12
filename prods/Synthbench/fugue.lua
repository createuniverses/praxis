-- fugue.lua

fugue = {}

fugue.cminscale = { 60, 62, 63, 65, 67, 68, 71, 72 }
fugue.cmajscale = { 60, 62, 64, 65, 67, 69, 71, 72 }

fugue.lastNoteTime = 0
fugue.timeBetweenNotes = 0.2
fugue.currnote = 1

function fugue.update()
  -- see how much time has elapsed
  -- play the next note.
  if os.clock() - fugue.lastNoteTime > fugue.timeBetweenNotes then
    print("Playing " .. fugue.cminscale[fugue.currnote])
    midiNoteOn(fugue.cminscale[fugue.currnote])
    fugue.currnote = fugue.currnote + 1
    if fugue.currnote > #fugue.cminscale then
      fugue.currnote = 1
    end
    fugue.lastNoteTime = os.clock()
  end
end

function fugue.render()
end
