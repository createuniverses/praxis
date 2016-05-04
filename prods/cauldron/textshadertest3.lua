glTexWBLoadFromString("abcd", 0)

glTexWBWriteToTexture(stringtex,2,12,4,1)
glTexWBWriteToTexture(stringtex)
compiletextshader()

setBufferName("textshadertest3.lua")

print2(getErrorText())
opengl-prelude.lua:5: GL_INVALID_OPERATION
stack traceback:
	[string "function onerrorgl(s) endGL() glResetStencil(..."]:1: in function <[string "function onerrorgl(s) endGL() glResetStencil(..."]:1>
	[C]: in function 'error'
	opengl-prelude.lua:5: in function 'assertgl'
	render_to_fbo.lua:82: in function 'render_to_fbo_with_input'
	textshader.lua:49: in function 'prerender'
	[string "prerender()"]:1: in main chunk

continue()
updatetextshadertext("hello")
continue()

do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w[1] = addButton(0,10,function(b) end)
w[2] = addButton(5,32,function(b) end)
--w[3] = redslider
w[3] = colorwheelgrp
w[4] = dome
end
