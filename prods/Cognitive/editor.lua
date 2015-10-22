-- editor.lua

keymap = {}

lastkeydown = -1

keymap[8] = {}
keymap[8][0] = function ()
  edBackspace()
end

keymap[46] = {}
keymap[46][0] = function ()
  edDelete()
end

keymap[9] = {}
keymap[9][0] = function ()
  for i = 1,2,1 do
    edInsertTextAt(" ", edGetPosition())
    edSetPosition(edGetRight(edGetPosition()))
  end
end

keymap[13] = {}
keymap[13][0] = function ()
  -- autoindent stuff
  -- shift/ctrl enter
  edInsertNewline()
end

-- arrow keys

keymap[37] = {}
keymap[37][0] = function ()
  -- ctrl: word or s-exp left
  -- shift: selection
  edSetPosition(edGetLeft(edGetPosition()))
end

keymap[39] = {}
keymap[39][0] = function ()
  edSetPosition(edGetRight(edGetPosition()))
end

keymap[38] = {}
keymap[38][0] = function ()
  edSetPosition(edGetUp(edGetPosition()))
end

keymap[40] = {}
keymap[40][0] = function ()
  edSetPosition(edGetDown(edGetPosition()))
end

function edTypeString(c)
  edInsertTextAt(c, edGetPosition())
  edSetPosition(edGetRight(edGetPosition()))
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

--newBuffer()
setBufferText("")
clearError()

print("Loaded editor.lua")

