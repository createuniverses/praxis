-- editor.lua

keymap = {}

function getKeyHandler(k,mods)
  local found = false
  if keymap[k] ~= nil then
    if keymap[k][mods] ~= nil then
      found = true
    end
  end
  if found == true then
    return keymap[k][mods].fn
  else
    return nil
  end
end

function setKeyHandler(k,mods,fn)
  if keymap[k] == nil then
    keymap[k] = {}
  end
  if keymap[k][mods] == nil then
    keymap[k][mods] = {}
  end
  keymap[k][mods].fn = fn
end

stdkeyids = {}
stdkeyids.backspace = 8
stdkeyids.delete = 46
stdkeyids.enter = 13
stdkeyids.tab = 9
stdkeyids.up = 38
stdkeyids.down = 40
stdkeyids.left = 37
stdkeyids.right = 39
--stdkeyids.f1 = 0
--stdkeyids.a = 0

if platform() == "windows" then
end

if platform() == "linux" then
  stdkeyids.backspace = 22
  stdkeyids.delete = 119
  stdkeyids.enter = 36
  stdkeyids.tab = 23
  stdkeyids.up = 111
  stdkeyids.down = 116
  stdkeyids.left = 113
  stdkeyids.right = 114
end

setKeyHandler(stdkeyids.backspace,0, function ()
  edBackspace()
end)

setKeyHandler(stdkeyids.delete,0, function ()
  edDelete()
end)

setKeyHandler(stdkeyids.tab,0, function ()
  for i = 1,2,1 do
    edInsertTextAt(" ", edGetPosition())
    edSetPosition(edGetRight(edGetPosition()))
  end
end)

setKeyHandler(stdkeyids.enter,0, function ()
  -- autoindent stuff
  -- shift/ctrl enter
  edInsertNewline()
end)

-- arrow keys

setKeyHandler(stdkeyids.left,  0, function ()
  -- ctrl: word or s-exp left
  -- shift: selection
  edSetPosition(edGetLeft(edGetPosition()))
end)

setKeyHandler(stdkeyids.right, 0, function ()
  edSetPosition(edGetRight(edGetPosition()))
end)

setKeyHandler(stdkeyids.up,    0, function ()
  edSetPosition(edGetUp(edGetPosition()))
end)

setKeyHandler(stdkeyids.down,  0, function ()
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
  print("onKeyDown " .. k)
  local mods = edGetKeyModifiers()
  if editorVisible() then
    local action = getKeyHandler(k, mods)
    if action ~= nil then
      onKeyDownSpecial = onKeyDownSpecial_Plain
      action()
    else
      print("Missing key handler for " .. k .. ", mods = " .. edGetKeyModifiers())
      replaceFunction("onKeyDownSpecial", onKeyDownSpecial_Plain,
        function (k2)
          print("  Setting key handler for " .. k .. ", mods " .. mods)
          print("  to print " .. string.char(k2) .. " (" .. k2 .. ")")
          setKeyHandler(k, mods,
            function ()
              edTypeString(string.char(k2))
            end)
        end, 1)
    end
  end
end

function onKeyUp(k)
  print("onKeyUp   " .. k)
end

function onKeyDownSpecial_Plain(k)
  print("  onKeyDownSpecial " .. k)
end

onKeyDownSpecial = onKeyDownSpecial_Plain

function onKeyUpSpecial(k)
  print("  onKeyUpSpecial " .. k)
end

--newBuffer()
setBufferText("")
clearError()

print("Loaded editor.lua")

