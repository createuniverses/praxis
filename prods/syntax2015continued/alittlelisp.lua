setBufferName("alittlelisp.lua")

print2(getFunction(render))
function render()
  WidgetLib.renderAll()

  --renderGreets2()

  glColor(150 + math.random(100),110,20)

  -- move this to airplane.renderGlobal
  renderStreamer(airplane.lwing)
  renderStreamer(airplane.rwing)
  lisp("(render)")

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
  
  trace2()
end

airplane.followcam = false
airplane.followcam = true

do
lisp([[
(define (render)
  (draw-line 0 0 0 100 0 100))
]])
end



print2(lisp("(+ 1 2 3)"))
6


do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
--w[1] = addButton(0,10,function(b) print("hello") end)
w[2] = addButton(5,32,function(b) end)
--w[3] = redslider
w[3] = colorwheelgrp
--w[4] = dome
end

disableStdMouseCam()
