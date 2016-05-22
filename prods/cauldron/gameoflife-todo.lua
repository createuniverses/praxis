
print2(getFunction(f5Pressed))
function f5Pressed()
  continue()
  dofile("fbotest.lua")
  dofile("mainshader.lua")
  dofile("render_to_fbo.lua")
  dofile("gameoflife.lua")
end

print2(getFunction(f10Pressed))
function f10Pressed() end

enableStdMouseCam()
disableStdMouseCam()


--print2(getFunction(render))
function render()
  WidgetLib.renderAll()

  --renderGreets2()
  --renderskythings()
  trace2()
  drawText2D("Buffer: " .. getBufferName(), 5,2)
end

-- On error keep rendering, but use the previous render function

-- Or at least a render function that shows the error and trace

-- Every frame save the old render function at the end of the render if there was no error.

-- setBufferName("thoughts.lua")


-- print2(getBufferName())
-- setBufferName("gameoflife.lua")


-- continue()

-- print2(getErrorText())
-- clearError()

-- setBufferName("gameoflife-todo.lua")
