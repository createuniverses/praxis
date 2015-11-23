--setBufferName("shiftenter.lua")

codelog = {}

do
 setKeyHandlerProgram(keymap, stdkeyids.enter,     1,
  [[
    local sCode = edGetLuaBlock()
    local p1,p2 = edGetLuaBlockPosition()
    if sCode == "" then
      sCode = getEditorLineText()
      p2 = getEditorLineEnd()
    end
    edSetPosition(p2)
    edInsertNewline()
    luaCall(sCode)
    table.insert(codelog, sCode)
    print("Done.")
  ]])
end

do
 setKeyHandlerProgram(keymap, stdkeyids.enter,     2,
  [[
    local sCode = edGetLuaBlock()
    if sCode == "" then
      sCode = getEditorLineText()
    end
    luaCall(sCode)
    table.insert(codelog, sCode)
    print("Done.")
  ]])
end

--print2(codelog[#codelog])

function makeCodeLogBuffer()
  parentBufferName = getBufferName()
  local sText = ""
  for i=1,#codelog,1 do
    sText = sText .. "Entry " .. i .. "\n"
    sText = sText .. codelog[i] .. "\n\n"
  end
  newBuffer("codelog.txt")
  setBufferText(sText)
  edSetPosition(#sText - 1)
end

showTrace()
print("codelogger logging")
--clearTrace()

