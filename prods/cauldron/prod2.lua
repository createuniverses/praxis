function renderskythings()
  glColor(150 + math.random(100),110,20)

  -- move this to airplane.renderGlobal
  renderStreamer(airplane.lwing)
  renderStreamer(airplane.rwing)

  for i=1,#skythings,1 do
    local thing = skythings[i]
    glPushMatrix()
      glTranslate(Vector3D.getArgs(thing.p))
      glColor(200,200,0)
      glutSolidSphere(thing.r)
      glColor(100,100,100)
      glutWireSphere(thing.r + 1)
    glPopMatrix()
  end
end

function render()
  WidgetLib.renderAll()

  --renderGreets2()
  --renderskythings()
  trace2()
end

setPickSphere(false)
hideFPS()
hideEditor()

dofile("server.lua")
dofile("server2.lua")

--dofile("sandbox.lua")
dofile("namespace.lua")

edSetRenderMode(2)
edSetVisColumns(60)
edSetVisLines(22)

--makeSandbox("turtle")
--useSandbox("turtle")

dofile("turtle.lua")
dofile("turtletest.lua")



