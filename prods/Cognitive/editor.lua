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
    if keymap[k][mods].fn == nil then
      keymap[k][mods].fn = loadstring(keymap[k][mods].program)
    end
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

function setKeyHandlerProgram(k,mods,prog)
  local fn = loadstring(prog)
  setKeyHandler(k, mods, fn)
  keymap[k][mods].program = prog
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

function edTab()
  for i = 1,2,1 do
    edInsertTextAt(" ", edGetPosition())
    edSetPosition(edGetRight(edGetPosition()))
  end
end

setKeyHandlerProgram(stdkeyids.backspace, 0, [[edBackspace()]])
setKeyHandlerProgram(stdkeyids.delete,    0, [[edDelete()]])
setKeyHandlerProgram(stdkeyids.tab,       0, [[edTab()]])

-- autoindent stuff
-- shift/ctrl enter
setKeyHandlerProgram(stdkeyids.enter,     0, [[edInsertNewline()]])

-- arrow keys
-- ctrl: word or s-exp left
-- shift: selection
setKeyHandlerProgram(stdkeyids.left,  0, [[edSetPosition(edGetLeft(edGetPosition()))]])
setKeyHandlerProgram(stdkeyids.right, 0, [[edSetPosition(edGetRight(edGetPosition()))]])
setKeyHandlerProgram(stdkeyids.up,    0, [[edSetPosition(edGetUp(edGetPosition()))]])
setKeyHandlerProgram(stdkeyids.down,  0, [[edSetPosition(edGetDown(edGetPosition()))]])

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
          local stringrep = string.char(k2)
          if string.char(k2) == "\\" then
            stringrep = "\\\\"
          end
          setKeyHandlerProgram(k, mods,
            [[edTypeString("]]..stringrep .. [[")]])
        end, 1)
          --setKeyHandler(k, mods,
          --  function ()
          --    edTypeString(string.char(k2))
          --  end)
        --end, 1)
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

