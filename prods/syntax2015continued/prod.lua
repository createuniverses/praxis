-- Name: prod.lua

inspect = require 'inspect'

dofile("util.lua")

dofile("unpack2.lua")
dofile("reflect.lua")
dofile("colorwheel.lua")

dofile("geometry.lua")
dofile("drawing.lua")
dofile("queue.lua")
dofile("widgets.lua")
dofile("slider.lua")

dofile("replacefn.lua")
dofile("editor.lua")
dofile("keymap.lua")

dofile("fnkeys.lua")

dofile("shiftenter.lua")

dofile("flight.lua")
dofile("spirograph3d.lua")
dofile("arch.lua")

dofile("skythings.lua")

dofile("greets.lua")

dofile("tweaks.lua")

dofile("dome.lua")
dofile("camwidget.lua")

dofile("streamer.lua")

function OnMouseMove(dx,dy,x,y)
  WidgetLib.callAllInRange("mousemove")
end

function LMBDown(x,y)
  --showTrace()
  --print(getMp3Time())
  WidgetLib.callAllInRange("lmbdown")
end

function LMBUp(x,y)
  WidgetLib.callAllInRange("lmbup")
end

function RMBDown(x,y)
  WidgetLib.callAllInRange("rmbdown")
end

function RMBUp(x,y)
  WidgetLib.callAllInRange("rmbup")
end

function render()
  WidgetLib.renderAll()

  --renderGreets2()

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
  
  trace2()
end

function update()
  WidgetLib.callAll("update")

  for i=1,#skythings,1 do
    local thing = skythings[i]
    local planepos = vec3d(transform.getTranslation(airplane.lspace))
    local tween = thing.p - planepos
    local dist = Vector3D.magnitude(tween)
    tween = Vector3D.normalize(tween)
    if dist < (30 + thing.r) then
      thing.p = planepos + (tween * (30+thing.r))
    end
  end
end

dofile("widgetgroup.lua")
dofile("colorwheelwidget.lua")

dofile("uiwidgets.lua")

do
Widgets = {}
Widgets[1] = spirowidget
Widgets[2] = airplane
Widgets[3] = uimainwidget
end

setBufferText([[
do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w[1] = addButton(0,10,function(b) end)
w[2] = addButton(5,32,function(b) end)
--w[3] = redslider
w[3] = colorwheelgrp
w[4] = dome
end
]])

dofile("trace2.lua")

showdiscs = true
showarms = true
airplane.followcam = true

--airplane.pilot = airplane.allstoppilot

function switchToManualPilot()
  airplane.pilot = function (o) end
  editorVisible = mainEditorVisible
  airplane.turnhead = false
  initPlaneControls()
end

function switchToAutoPilot()
  editorVisible = function () return true end
  airplane.pilot = airplane.normalpilot
end

switchToAutoPilot()
--switchToManualPilot()

enableStdMouseCam()

clearError()




