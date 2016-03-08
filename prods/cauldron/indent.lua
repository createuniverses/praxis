-- indent.lua

function edGetIndent()
  local indent = 0
  local pos = getEditorLineStart()
  while edGetAt(pos) == " " do
    indent = indent + 1
    pos = pos + 1
  end
  return indent
end

function edGetIndent()
  local indent = 0
  local pos = getEditorLineStart()
  while (function ()
           local c = edGetAt(pos)
           if c == "\n" then return false end
           return c == " "
         end)() do
    indent = indent + 1
    pos = pos + 1
  end
  return indent
end

