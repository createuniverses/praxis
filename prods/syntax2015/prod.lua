-- Name: prod.lua

inspect = require 'inspect'

function linearInterpolate(inmin, inmax, outmin, outmax, val)
  local proportion = (val - inmin) / (inmax - inmin)
  local out = proportion * (outmax - outmin) + outmin
  return out
end

function lookDown()
  pos = { getCamPos() }
  -- pos[1] = pos[1] + 10 -- look along x
  pos[3] = pos[3] + 10 -- look along z
  lookAt(table.unpack(pos))
  rotateCam(0, math.pi * -0.5)
end

function deg2rad(a)
  return math.pi * (a/180)
end

function rad2deg(a)
  return 180 * (a/math.pi)
end


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
dofile("flight.lua")
dofile("spirograph3d.lua")
dofile("arch.lua")

showTrace()
function update()
  WidgetLib.callAll("update")
  if getMp3Time() > 17 then
    airplane.pilot = airplane.normalpilot
  end
  clearTrace()
  print(""..getMp3Time())
end

skythings = {}
function makeskythings()
 for i=1,17,1 do
  skythings[i] =
  { p = vec3d(math.random(1000) - 500,
              math.random(300) + 50,
              math.random(1000) - 500),
    r = math.random(10) + 2 }
 end
end

makeskythings()
makeskythings()

function render()
  WidgetLib.renderAll()

  --[[glColor(0, 0, 0)
  glBeginQuads()
    glVertex(-500,-2,-500)
    glVertex(-500,-2, 500)
    glVertex( 500,-2, 500)
    glVertex( 500,-2,-500)
  glEnd()]]

  glColor(150 + math.random(100),110,20)

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

function OnMouseMove(dx,dy,x,y)
  WidgetLib.callAllInRange("mousemove")
end

function LMBDown(x,y)
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

playMp3()
fullscreenMode()
hideEditor()
hideTrace()
hideError()


function makeCamPosSaver()
  local x,y,z = getCamPos()
  x = math.floor(x)
  y = math.floor(y)
  z = math.floor(z)
  local s = "setCamPos("..x..","..y..","..z..")"
  print2(s)
end
--makeCamPosSaver()
--print2(getMouseCursorPos())

do
  airplane.followcam = false
  setCamPos(-184,293,42)
  lookAt(225,0,300)
  showdiscs = true
  showarms = true
end

maxstreamersegments = 300

do
  airplane.followcam = true
  showdiscs = false
  showarms = false
end

hideFPS()
--showFPS()

airplane.pilot = airplane.normalpilot
airplane.pilot = function (o) airplane.takeoffpilot(o) controls.thrust = 0 end

function airplane.allstoppilot(o)
  airplane.takeoffpilot(o)
  controls.thrust = 0
end

--makePositionSaver("airplane")
do
  setCamPos(0,35,-30)
  lookAt(0,0,50)
  stopMp3()
  playMp3()
  airplane.lspace=transform.new()
  transform.translate(airplane.lspace, 0,10,0)
  airplane.pilot = airplane.takeoffpilot
end

--makeCamPosSaver()
setCamPos(0,35,-30)
lookAt(0,0,50)

showTrace()
hideTrace()

function update()
  WidgetLib.callAll("update")
  
  if getMp3Time() > 117 then
    airplane.followcam = false
    airplane.pilot = airplane.allstoppilot
  elseif getMp3Time() > 105 then
    airplane.followcam = true
    showdiscs = false
    showarms = false
  elseif getMp3Time() > 78 then
    do
      airplane.followcam = false
      lookAt(transform.getTranslation(airplane.lspace))
      showdiscs = true
      showarms = true
    end
  elseif getMp3Time() > 16 then
    airplane.pilot = airplane.normalpilot
  end
  --clearTrace()
  --print(""..getMp3Time())
  
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
  
  if isMp3Playing() == false then
    os.exit()
  end
end

setPickSphere(false)

takeoffthrust = 0.75

makeskythings()
