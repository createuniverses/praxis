renderfornext(function () drawLine(math.random(100),10,math.random(100),math.random(100),10,math.random(100)) end, 300)
setBufferName("pastime2.lua")

renderfns = {}

print2(getFunction(render))
function render()
  WidgetLib.renderAll()

  for k,v in pairs(renderfns) do
    renderfns[k]()
  end

  rt = rt + 5

  glPushMatrix()

  glRotate(rt,0,0,1)

  helix()

  glPopMatrix()

  helixsolid()

  trace2()
end


print2(math.random(100))
13




do
  closeBuffer()
  switchToBuffer("pastime.lua")
end
