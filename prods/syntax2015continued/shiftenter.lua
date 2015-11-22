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

showTrace()
print("codelogger logging")
--clearTrace()

