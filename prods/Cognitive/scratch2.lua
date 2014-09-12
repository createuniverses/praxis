fugue.currNote = {}

fugue.currNote[1] = 1
fugue.currNote[2] = 1

function midiUpdate()
  if fugue.playing == true then
    -- isn't the last note merely the note before the current one??
    if fugue.currnote <= #fugue.lines[1] then
      if fugue.p1On == true then
        local note = fugue.lines[1][fugue.currnote]
        local lastNote = fugue.lines[1][fugue.currnote-1]
        local lastNoteMidi
        if lastNote.rest ~= nil then
          lastNoteMidi = fugue.makeMidiNote(lastNote)
        end
        
        midiSelectInstrument(fugue.inst1)
        midiNoteOff(fugue.lastNote1)

        if note.rest ~= nil then
          local midiNote = fugue.makeMidiNote(note)
          --print(fugue.currnote, midiNote)

          midiSelectInstrument(fugue.inst1)
          midiNoteOff(fugue.lastNote1)
          midiNoteOn(midiNote)
          
          -- play note in the other line.
          
          -- local duration = 100 * fugue.rhythms[2][((fugue.currnote - 1) % 4) + 1]
          -- print(duration)
          
          if note.duration ~= nil then
            midiLaunchNextEvent(note.duration)
          else
            midiLaunchNextEvent(100) -- backward compatibility
          end
          
          fugue.lastNote1 = midiNote
        end
        
        fugue.currnote = fugue.currnote + 1
      end
    else
      midiNoteOff(fugue.lastNote1)
      print("no more notes!")
      fugue.playing = false
    end
  end
end

