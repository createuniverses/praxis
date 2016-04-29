
do
 local sti = 1
 function f8Pressed()
 print(sti)
 stringtex = glStringToTexture(
  string.char(sti) .. "  a",
  GL_RGBA8UI_EXT,
  GL_RGBA_INTEGER_EXT,
  GL_UNSIGNED_BYTE)
 sti = sti + 1
 if sti > 255 then sti = 1 end
 end
end

print2(string.byte('a'))
97

80

stringtex = glStringToTexture("hello")
print2(fbotest.fboId)
48
continue()

do
 stringtex = glStringToTexture(
  "[Greetings] Hello there, giddy restlessness ",
  --"Successful test!! ",
  --readFile("prod.lua"),
  GL_RGBA8UI_EXT,
  GL_RGBA_INTEGER_EXT,
  GL_UNSIGNED_BYTE)
end

clearError()

do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w[1] = addButton(0,10,function(b) end)
w[2] = addButton(5,32,function(b) end)
--w[3] = redslider
w[3] = colorwheelgrp
w[4] = dome
end



