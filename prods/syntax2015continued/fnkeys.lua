function f2Pressed()
  local name = getBufferName()
  newBuffer(getBufferName() .. " - command")
  print2("")
  print2("")
  print2("")
  print2("")
  print2("")
  print2([[do]])
  --print2([[  local name = getParentBufferName()]])
  print2([[  closeBuffer()]])
  print2([[  switchToBuffer("]] .. name .. [[")]])
  print2([[end]])
  edSetPosition(0)
end

function f3Pressed()
  local filename = getSelectedText()
  newBuffer()
  loadBuffer(filename)
end

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


function f11Pressed()
  if editorVisible() then
    hideEditor()
  else
    showEditor()
  end
end

function f8Pressed()
  dofile("syntax2015.lua")
end



