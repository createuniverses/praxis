print2(midiGetInputPortCount())
for i=0,midiGetInputPortCount()-1,1 do
  print2(midiGetInputPortName(i))
end
Midi Through:0
USB Uno MIDI Interface:0

midiOpenInputPort()

midiin = {midiInputPortMessage()}

clearTrace()
do
  midiin = {midiInputPortMessage()}
  print(#midiin)
  for i=1,#midiin,1 do
    print(midiin[i])
  end
end


function initRecorder()
  local recorder = {}
  recorder.framenum = 0
  recorder.notes = {}
  recorder.currnote = 1
  return recorder
end

recorder = initRecorder()

print2(recorder.framenum)

recorder.framenum = 0
recorder.currnote = 1

print2(recorder.currnote)
print2(#recorder.notes)
14

recorder.update = recorder.record
recorder.update = recorder.playback
recorder.update = function (r) end

function beginRecording()
  recorder.framenum = 0
  recorder.notes = {}
  recorder.currnote = 1
  recorder.update = recorder.record
end

function beginPlayback()
  recorder.framenum = 0
  --recorder.notes = {}
  recorder.currnote = 1
  recorder.update = recorder.playback
end

function stopRecorder()
  recorder.update = function (r) end
end

function recorder.record(r)
  local midiin = {midiInputPortMessage()}
  if #midiin > 0 then
    local rec = { midi = midiin, frame = r.framenum }
    table.insert(r.notes, rec)
  end
  r.framenum = r.framenum + 1
end

function recorder.playback(r)
  if r.currnote <= #r.notes then
    local note = r.notes[r.currnote]
    if r.framenum == note.frame then
      if note.midi[1] == 144 then -- note on
        --print("Note on: ".. note.midi[2])
        midiNoteOn(note.midi[2])
      end
      if note.midi[1] == 128 then -- note off
        --print("Note off: ".. note.midi[2])
        midiNoteOff(note.midi[2])
      end
      r.currnote = r.currnote + 1
    end
  end
  r.framenum = r.framenum + 1
end


print2(getFunction("update"))
function update()
  WidgetLib.callAll("update")
  recorder:update()
  
  fugue.update()
  
  SynthNode.updateSynthNode(sineConNode)
  SynthNode.updateSynthNode(sineConNode2)
  SynthNode.updateSynthNode(sineGenNode)
  SynthNode.updateSynthNode(lpfEffNode)
  SynthNode.updateSynthNode(sinkNode)
  
  g_updateCount = g_updateCount + 1
  fugue.timeBetweenNotes = 0.1
  
end
