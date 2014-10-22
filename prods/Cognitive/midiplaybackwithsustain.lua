function recorder.playback(r)
    if r.currnote <= #r.notes then
      local note = r.notes[r.currnote]
      while note ~= nil and r.framenum == note.frame do
      --if r.framenum == note.frame then
        if note.midi[1] == 144 then -- note on
          --print("Note on: ".. note.midi[2])
          --midiNoteOn(note.midi[2], note.midi[3])
          midiNoteOn(note.midi[2])
        end
        if note.midi[1] == 128 then -- note off
          --print("Note off: ".. note.midi[2])
          if r.sustain == false then
            midiNoteOff(note.midi[2])
          else
            table.insert(r.sustainednotes, note.midi[2])
          end
        end
        
        if note.midi[1] == 176 and note.midi[3] == 127 then -- sustain pedal down
          print("sustain on")
          r.sustain = true
          r.sustainednotes = {}
        end
        
        if note.midi[1] == 177 and note.midi[3] == 0 then -- sustain pedal down
          print("sustain off")
          r.sustain = false
          for i=1,#r.sustainednotes,1 do
            midiNoteOff(r.sustainednotes[i])
          end
        end
        
        r.currnote = r.currnote + 1
        note = r.notes[r.currnote]
      end
    end
    r.framenum = r.framenum + 1
end
