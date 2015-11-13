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

function update()
  WidgetLib.callAll("update")
end

skythings = {}
for i=1,15,1 do
  skythings[i] =
  { p = vec3d(math.random(1000) - 500,
              math.random(300) + 50,
              math.random(1000) - 500),
    r = math.random(10) + 2 }
end

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

--hideFPS()
showFPS()



