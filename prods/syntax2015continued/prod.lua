-- Name: prod.lua

inspect = require 'inspect'

dofile("util.lua")

dofile("unpack2.lua")
dofile("reflect.lua")
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

  if stage == 0 then
    renderGreets2()
  end

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

function update()
  WidgetLib.callAll("update")

  if stage == 4 then
    airplane.followcam = false
    airplane.pilot = airplane.allstoppilot
  elseif stage == 3 then
    airplane.followcam = true
    showdiscs = false
    showarms = false
  elseif stage == 2 then
    airplane.followcam = false
    showdiscs = true
    showarms = true
  elseif stage == 1 then
    airplane.pilot = airplane.normalpilot
    showdiscs = false
    showarms = false
  else
    airplane.pilot = airplane.takeoffpilot
    showdiscs = false
    showarms = false
  end
  
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
