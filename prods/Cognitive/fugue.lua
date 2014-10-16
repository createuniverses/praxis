-- fugue.lua

fugue = {}

dofile("fugue_base.lua")
dofile("fugue_data.lua")

dofile("fugue_compose.lua")
dofile("fugue_render.lua")

fugue.currNote = nil
fugue.nextNoteID = 1
fugue.noteCountdown = 0
fugue.defaultDuration = 200
fugue.playing = false

function fugue.update()
  if fugue.currnote + 10 > #fugue.lines[1] then
    --fugue.improvise(1)
  end
end

function fugue.render()
  fugue.renderSheet(50, 16)
  --fugue.renderGear(80,50,-160, 50, 1, { red = 50, green = 50, blue = 150 + math.random(20) }, 50, 16 )
  --fugue.renderGear(80,50,-262, 50, 2, { red = 150 + math.random(20), green = 50, blue = 50 }, 50, 16 )
  --fugue.renderDemiurge()
end

function fugue.makeMidiNote(note)
  --print2(inspect(note))
  if note == nil then return nil end
  
  -- note is a degree of the scale and an octave
  local midiNote = fugue.currentKey +
                   note.octave * 12 +
                   fugue.minscale[note.degree]
  
  return midiNote
end

-- at the moment, turnOffNotes and turnOnNotes
-- use only one line.

function fugue.turnOffNotes()
  if fugue.currNote ~= nil then
    fugue.noteCountdown = fugue.noteCountdown - midiGetInterval()
    if fugue.noteCountdown <= 0 then
      if fugue.currNote.rest == nil then
        local midiNote = fugue.makeMidiNote(fugue.currNote)
        midiNoteOff(midiNote)
      end
      fugue.currNote = nil
    end
  end
end

function fugue.turnOnNotes()
  if fugue.currNote == nil then
    if fugue.nextNoteID <= #fugue.lines[1] then
      fugue.currNote = fugue.lines[1][fugue.nextNoteID]
      local midiNote = fugue.makeMidiNote(fugue.currNote)
      
      midiSelectInstrument(fugue.inst1)
      
      if fugue.currNote.rest == nil then
        midiNoteOn(midiNote)
      end
      
      if fugue.currNote.duration ~= nil then
        fugue.noteCountdown = fugue.currNote.duration
      else
        fugue.noteCountdown = fugue.defaultDuration
      end
      
      fugue.nextNoteID = fugue.nextNoteID + 1
    end
  end
end

function midiUpdate()
  fugue.turnOffNotes()
  if fugue.playing == true then
    fugue.turnOnNotes()
  end
end

function fugue.play()
  fugue.nextNoteID = 1
  fugue.noteCountdown = 0
  fugue.playing = true
end

function fugue.stop()
  fugue.playing = false
end
