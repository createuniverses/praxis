print2(getFunction(f3Pressed))
function f3Pressed()
 local name = getBufferName()
 newBuffer(getBufferName() .. " - command")
 local s = [[
  local name = "]] .. name .. [["
  closeBuffer()
  switchToBuffer(name)
  local text = getBufferText()
  local pos = string.find(text, "", edGetPosition())
  edSetPosition(pos-1)]]
 setBufferText(s)
 edSetPosition(131)
end

-- make it start from the end

setBufferName("f3search.lua")




do
  closeBuffer()
  switchToBuffer("shader-fluid.lua - command")
end
