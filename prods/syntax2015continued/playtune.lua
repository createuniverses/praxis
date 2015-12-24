function playTune(...)
  local tune = {...}
  local step = 0.1
  for i=1,#tune,1 do
    local note = tune[i]
    local fn1 = function () midiNoteOn(note) end
    local fn2 = function () midiNoteOff(note) end
    addEvent((i-1) * step, fn1)
    addEvent(    i * step, fn2)
  end
  --addEvent(step * #tune,
  --  function () playTune(table.unpack(tune)) end)
end

--print2(table.unpack({1,2,3}))

--midiStart()

playTune(60, 63, 62, 63, 67, 63, 62, 63)


--midiNoteOn(60)
--midiNoteOff(60)


--clearTrace()

