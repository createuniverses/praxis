-- editor.lua

keymap = {}

lastkeydown = -1

function getKeyHandler(k,mods)
  if keymap[k] ~= nil then
    return keymap[k][mods]
  else
    return nil
  end
end

function setKeyHandler(k,mods,fn)
  if keymap[k] == nil then
    keymap[k] = {}
  end
  keymap[k][mods] = fn
end

setKeyHandler(8,0, function ()
  edBackspace()
end)

setKeyHandler(46,0, function ()
  edDelete()
end)

setKeyHandler(9,0, function ()
  for i = 1,2,1 do
    edInsertTextAt(" ", edGetPosition())
    edSetPosition(edGetRight(edGetPosition()))
  end
end)

setKeyHandler(13,0, function ()
  -- autoindent stuff
  -- shift/ctrl enter
  edInsertNewline()
end)

-- arrow keys

setKeyHandler(37,0, function ()
  -- ctrl: word or s-exp left
  -- shift: selection
  edSetPosition(edGetLeft(edGetPosition()))
end)

setKeyHandler(39,0, function ()
  edSetPosition(edGetRight(edGetPosition()))
end)

setKeyHandler(38,0, function ()
  edSetPosition(edGetUp(edGetPosition()))
end)

setKeyHandler(40,0, function ()
  edSetPosition(edGetDown(edGetPosition()))
end)

function edTypeString(c)
  edInsertTextAt(c, edGetPosition())
  local l = string.len(c)
  for i=1,l,1 do
    edSetPosition(edGetRight(edGetPosition()))
  end
end

function edGetKeyModifiers()
  local n = 0
  if isShiftDown()  then n = n + 1 end
  if isCtrlDown()   then n = n + 2 end
  if isAltDown()    then n = n + 4 end
  return n
end

function onKeyDown(k)
  if editorVisible() then
    local action = keymap[k]
    if action ~= nil then
      local mods = edGetKeyModifiers()
      action = action[mods]
      if action ~= nil then
    	  action()
      end
	  end
  end
  lastkeydown = k
  print("onKeyDown " .. k)
end

function onKeyUp(k)
  print("onKeyUp   " .. k)
end

function onKeyDownSpecial(k)
  if lastkeydown ~= -1 then
    if keymap[lastkeydown] == nil then
      keymap[lastkeydown] = {}
    end
    local mods = edGetKeyModifiers()
    if keymap[lastkeydown][mods] == nil then
      print(lastkeydown .. " with mods = " .. mods .. " maps to " .. string.char(k))
      keymap[lastkeydown][mods] = function () edTypeString(string.char(k)) end
    end
  end
  print("     onKeyDownSpecial " .. k)
end

function onKeyUpSpecial(k)
  print("-----onKeyUpSpecial " .. k .. "------")
end

function onKeyDown(k)
  edTypeString(" kd " .. k)
end

function onKeyUp(k)
  edTypeString(" ku " .. k .. "\n")
end

function onKeyDownSpecial(k)
  edTypeString(" kds " .. k .. "'" .. string.char(k) .. "'")
end

function onKeyUpSpecial(k)
  edTypeString(" kus " .. k .. "\n")
end

--newBuffer()
setBufferText("")
clearError()

print("Loaded editor.lua")

