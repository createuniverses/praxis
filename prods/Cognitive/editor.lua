-- editor.lua

keymap = {}


function onKeyDown(k)
  print("onKeyDown " .. k)
end

function onKeyUp(k)
  print("onKeyUp   " .. k)
end

function onKeyDownSpecial(k)
  if editorVisible() then
    edInsertTextAt(string.char(k), edGetPosition())
    edSetPosition(edGetRight(edGetPosition()))
  end
  print("     onKeyDownSpecial " .. k)
end

function onKeyUpSpecial(k)
  print("------onKeyUpSpecial " .. k .. "-------")
end

newBuffer()
print("Loaded editor.lua")

