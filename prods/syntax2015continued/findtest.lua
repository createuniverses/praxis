



  do
    local name = "prod.lua"
    --closeBuffer()
    switchToBuffer(name)
    local text = getBufferText()
    local pos = string.find(text, "takeoff", edGetPosition())
    edSetPosition(pos)
  end

function f3Pressed()
  switchToBuffer("findtest.lua")
end


