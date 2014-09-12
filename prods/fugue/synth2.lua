
function makeElement()
  local element = {}
  element.buffer = List.new()
  element.rbuffer = List.new()
  -- inputA, inputB, update and render
  return element
end

elementFunctions = {}

elementFunctions.getData(e,i)
  local j = e.frameStart + i
  if j > g_elementBufferSize then j = j - g_elementBufferSize end
  return e.buffer[j]
end

elementfunctions.updateCenter(e,n)
  for i=1,n,1 do
    writeSample(e.buffer[e.frameStart + i])
  end
end


elementFunctions.generateSine = function (e)
  e.angle = e.angle + (2 * math.pi) * (e.frequency / g_sampleRate)
  if e.angle > 2 * math.pi then e.angle = e.angle - (2 * math.pi) end
  local pos = e.begin
  for i=1,1024,1 do
    element.buffer[pos] = e.amplitude * math.sin(e.angle)
    pos = pos + 1
    if pos > 2048 then pos = 1 end
  end
end

centerElement = makeElement()
sineElement = makeElement()
