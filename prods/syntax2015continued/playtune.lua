function playTune(...)
  local tune = {...}
  local step = 0.3
  for i=1,#tune,1 do
    local note = tune[i]
    local fn1 = function () midiNoteOn(note) end
    local fn2 = function () midiNoteOff(note) end
    addEvent((i-1) * step, fn1)
    addEvent(    i * step, fn2)
  end
end

playTune(60, 62, 64, 62, 60)

