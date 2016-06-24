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

function clamp(x,min,max)
  if x < min then return min end
  if x > max then return max end
  return x
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

  local r,g,b = getClearColor()
  r = clamp(r-20,0,255)
  g = clamp(g-20,0,255)
  b = clamp(b-20,0,255)
  setClearColor(r,g,b)
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

do
Widgets = {}
--Widgets["spiro"] = spirowidget
--Widgets["plane"] = airplane
Widgets["ui"] = uimainwidget
end

dofile("prod2.lua")

edSetRenderMode(2)
showFPS()
clearTrace()
clearError()

edNativeControlOn()

-- shader thing


function f4Pressed()
  continue()
  dofile("fbotest.lua")
  dofile("mouseshader.lua")
  dofile("fbotest3.lua")
end

function f5Pressed()
  continue()
  dofile("fbotest.lua")
  dofile("mainshader.lua")
  dofile("render_to_fbo.lua")
  dofile("gameoflife.lua")
end

function f6Pressed()
  continue()
  dofile("fbotest.lua")
  dofile("mainshader.lua")
  dofile("render_to_fbo.lua")
  dofile("shader-fluid.lua")
end

function f7Pressed()
  continue()
  --dofile("fbotest.lua")
  --dofile("mainshader.lua")
  --dofile("render_to_fbo.lua")
  dofile("textshader.lua")
end

--windowedMode(50,200,710,740)

dofile("opengl-prelude.lua")
dofile("textshader.lua")
setKeyRepeat(true)

--windowedMode(200, 80, 720, 620)
windowedMode(200, 80, 1200, 620)

--dofile("forthexample.lua")


loadBuffer("initial.lua")

hideFPS()
function trace2() end

edSetVisColumns(80)

dofile("errorhandling.lua")
dofile("update.lua")



