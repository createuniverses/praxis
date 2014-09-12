-- directcomposition.lua

praxis:

function midiUpdate()
  if fugue.playing == true then
    -- isn't the last note merely the note before the current one??
    if fugue.currnote <= #fugue.lines[1] then
      if fugue.p1On == true then
        local note = fugue.lines[1][fugue.currnote]
        local midiNote = fugue.makeMidiNote(note)
        local lastNote = fugue.lines[1][fugue.currnote-1]
        local lastNoteMidi = fugue.makeMidiNote(lastNote)
        
        midiSelectInstrument(fugue.inst1)
        
        if lastNoteMidi ~= nil then
          midiNoteOff(lastNoteMidi)
        end
        
        if note.rest == nil then
          --print(fugue.currnote, midiNote)
          
          midiNoteOn(midiNote)
          
          -- play note in the other line.
          
          -- local duration = 100 * fugue.rhythms[2][((fugue.currnote - 1) % 4) + 1]
          -- print(duration)
        end
        
        if note.duration ~= nil then
          midiLaunchNextEvent(note.duration)
        else
          midiLaunchNextEvent(100) -- backward compatibility
        end
        
        fugue.currnote = fugue.currnote + 1
      end
    else
      local lastNote = fugue.lines[1][fugue.currnote-1]
      local lastNoteMidi = fugue.makeMidiNote(lastNote)
      if lastNoteMidi ~= nil then
        midiNoteOff(lastNoteMidi)
      end
      print("no more notes!")
      fugue.playing = false
    end
  end
end




print2(getFunction("fugue.makeMidiNote"))

function fugue.makeMidiNote(note)
  --print2(inspect(note))
  if note == nil then return nil end
  
  -- note is a degree of the scale and an octave
  local midiNote = fugue.currentKey +
                   note.octave * 12 +
                   fugue.minscale[note.degree]
  
  
  return midiNote
end




do
fugue.lines[1] = {}
fugue.currnote = 1
fugue.beatDur = 500

table.insert(fugue.lines[1],
  { degree = 7, octave = -1, duration = fugue.beatDur * 0.5 })

table.insert(fugue.lines[1],
  { degree = 1, octave = 0, duration = fugue.beatDur * 1 })
table.insert(fugue.lines[1],
  { degree = 2, octave = 0, duration = fugue.beatDur * 0.5 })
table.insert(fugue.lines[1],
  { degree = 3, octave = 0, duration = fugue.beatDur * 1 })

table.insert(fugue.lines[1],
  { degree = 1, octave = 0, rest = true, duration = fugue.beatDur * 3.5 })

table.insert(fugue.lines[1],
  { degree = 4, octave = 0, duration = fugue.beatDur * 0.5 })
table.insert(fugue.lines[1],
  { degree = 5, octave = 0, duration = fugue.beatDur * 1 })

fugue.play()
end

print("Remember to pretend that this is the 3D Construction Kit")




