print2(getFunction(f5Pressed))
function f5Pressed()
do
  continue()
  dofile("fbotest.lua")
  dofile("render_to_fbo.lua")
  dofile("shader-fluid.lua")
  --dofile("gameoflife.lua")
  --dofile("fbotest3.lua")
end

dofile("shader-fluid.lua")
continue()
clearError()

enableStdMouseCam()

lookDown()
disableStdMouseCam()

setBufferName("mainshader-test.lua")

function f6Pressed()
 dofile("shader-fluid.lua")
 dofile("mainshader.lua")
 continue()
end

do
  dofile("mainshader.lua")
 continue()
end

hideTrace()

do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w[1] = addButton(0,10,function(b) end)
w[2] = addButton(5,32,function(b) end)
--w[3] = redslider
w[3] = colorwheelgrp
w[4] = dome
end

