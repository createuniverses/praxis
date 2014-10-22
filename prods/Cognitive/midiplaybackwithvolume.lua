

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
          midiNoteOff(note.midi[2])
        end
        
        r.currnote = r.currnote + 1
        note = r.notes[r.currnote]
      end
    end
    r.framenum = r.framenum + 1
end

function recorder.record(r)
    local midiin = {midiInputPortMessage()}
    while #midiin > 0 do
    --if #midiin > 0 then
      local rec = { midi = midiin, frame = r.framenum }
      table.insert(r.notes, rec)
      midiin = {midiInputPortMessage()}
    end
    r.framenum = r.framenum + 1
end

do
  i = 10
  while i>0 do
    print2(i)
    i = i - 1
  end
end
10
9
8
7
6
5
4
3
2
1

