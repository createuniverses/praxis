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


recorder = {}
function recorder.update(r)
  local midiin = {midiInputPortMessage()}
  if #midiin == 3 then
    if midiin[1] == 144 then -- note on
    end
    if midiin[1] == 128 then -- note off
      
    end
  end
end
