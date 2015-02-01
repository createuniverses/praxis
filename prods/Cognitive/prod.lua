-- Name: prod.lua

inspect = require 'inspect'

function linearInterpolate(inmin, inmax, outmin, outmax, val)
  local proportion = (val - inmin) / (inmax - inmin)
  local out = proportion * (outmax - outmin) + outmin
  return out
end

dofile("unpack2.lua")
dofile("reflect.lua")
dofile("geometry.lua")
dofile("drawing.lua")
dofile("queue.lua")
dofile("widgets.lua")
dofile("slider.lua")
dofile("synth.lua")
dofile("fugue.lua")
dofile("cognitive.lua")
dofile("gears.lua")

dofile("transformtest.lua")

dofile("main.lua")

g_updateCount = 0

setMaxFramerate(50)

--windowedMode(-900,10,800,450)
--windowedMode(-1000,100)
--windowedMode()
--fullscreenMode()

-- setBufferText("dofile(\"prod.lua\")")
setCamPos(0,90,100)

playSound()
stopSound()

-- midiLaunchNextEvent is called to start the midi "engine"

math.randomseed(os.time())

midiStart()
midiOpenInputPort()

--midiLaunchNextEvent(100)

fugue.compose()

--setFloorGrid(false)
--setProbesHUD(false)

--setPickSphere(false)
--hideTrace()
--hideEditor()

--loadBuffer("scratch.lua")

setCamPos(-180, 220, -240)
lookAt(60,0,-200)

clearTrace()

-- update, render and all input callbacks should all call those respective functions
-- for all registered objects.

dofile("scratch.lua")

function lookDown()
  pos = { getCamPos() }
  -- pos[1] = pos[1] + 10 -- look along x
  pos[3] = pos[3] + 10 -- look along z
  lookAt(table.unpack(pos))
  rotateCam(0, math.pi * -0.5)
end

function moveWindowLeft()
  windowedMode(0,0, getWinScreenWidth() * 0.5, getWinScreenHeight())
end

function moveWindowRight()
  windowedMode(getWinScreenWidth() * 0.5,0, getWinScreenWidth() * 0.5, getWinScreenHeight())
end

function moveWindowBottomLeft()
  windowedMode(0,getWinScreenHeight() * 0.5, getWinScreenWidth() * 0.5, getWinScreenHeight() * 0.5)
end

function moveWindowTopLeft()
  windowedMode(0,0, getWinScreenWidth() * 0.5, getWinScreenHeight() * 0.5)
end

function moveWindowTop()
  windowedMode(0,0, getWinScreenWidth(), getWinScreenHeight() * 0.3)
end

function moveWindowBottomRight()
  windowedMode(getWinScreenWidth() * 0.5,getWinScreenHeight() * 0.5, getWinScreenWidth() * 0.5, getWinScreenHeight() * 0.5)
end

function moveWindowTopRight()
  windowedMode(getWinScreenWidth() * 0.5,0, getWinScreenWidth() * 0.5, getWinScreenHeight() * 0.5)
end

function moveWindowFull()
  windowedMode(0,0, getWinScreenWidth(), getWinScreenHeight())
end

--os.exit()

--moveWindowFull()
moveWindowLeft()

turnOnBorders()
--windowedMode(100,50,800,600)
windowedMode(100,100,700,600)
--windowedMode(1200,400,700,600)
-- Calling turnOnBoarders after could result in bad
-- window size, because the size hasn't been set by the time
-- turnOnBorders is called.

loadBuffer("s7test.lua")
newBuffer()
loadBuffer("transformtest2.lua")
newBuffer()
loadBuffer("slidertest.lua")
newBuffer()
loadBuffer("camerabasetest.lua")
newBuffer()
loadBuffer("randomwidget.lua")
newBuffer()
loadBuffer("camtransformwidget.lua")
newBuffer()
loadBuffer("stenciltest.lua")
newBuffer()
loadBuffer("portalwidget.lua")
--newBuffer()
--loadBuffer("findtest.lua")

dofile("portalwidget.lua")

portal = WidgetLib.newPortal()
portal = WidgetLib.newPortal()
portal = WidgetLib.newPortal()

hideEditor()

edSetRenderMode(2)

dofile("server_multi.lua")
