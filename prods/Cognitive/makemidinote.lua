praxis: setClipboardText(getFunction("fugue.makeMidiNote"))

praxis:

function fugue.makeMidiNote(note)
  if note == nil then return nil end
  if note.rest ~= nil then return nil end
  
  -- note is a degree of the scale and an octave
  local midiNote = fugue.currentKey +
                   note.octave * 12 +
                   fugue.minscale[note.degree]
  
  
  return midiNote
end


        -- this is a standard lua idiom
        -- check member and check parent
        
        
        if lastNote ~= nil then
          if lastNote.rest ~= nil then
            lastNoteMidi = fugue.makeMidiNote(lastNote)
          end
        end
