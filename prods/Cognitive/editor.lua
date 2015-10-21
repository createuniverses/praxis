-- editor.lua

keymap = {}

lastkeydown = -1

function onKeyDown(k)
  if editorVisible() then
    local char = keymap[k]
    if char ~= nil then
      edInsertTextAt(char, edGetPosition())
      edSetPosition(edGetRight(edGetPosition()))
    end
  end
  lastkeydown = k
  --print("onKeyDown " .. k)
end

function onKeyUp(k)
  print("onKeyUp   " .. k)
end

function onKeyDownSpecial(k)
  if lastkeydown ~= -1 then
    if keymap[lastkeydown] == nil then
      print(k .. " maps to " .. string.char(k))
      keymap[lastkeydown] = string.char(k)
    end
  end
  --print("     onKeyDownSpecial " .. k)
end

function onKeyUpSpecial(k)
  print("-----onKeyUpSpecial " .. k .. "------")
end

newBuffer()
print("Loaded editor.lua")

