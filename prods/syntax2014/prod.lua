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
dofile("drawto.lua")
dofile("intersection.lua")

dofile("timeline.lua")

dofile("track.lua")
dofile("mech.lua")

function render()
    timeline[currentTimelineItem].effect.render()
end

function update()
    if isMp3Playing() == false then
        os.exit()
    end

    if isTransitionDue() then
        nextScene()
    end
    
    timeline[currentTimelineItem].effect.update()
end

initTimeline()

--turnOnBorders()
--windowedMode(0,0,800,600)

edSetRenderMode(0)


function f12Pressed() hideEditor() end
function f11Pressed() showEditor() end

function f10Pressed() print2(getMp3Time()) saveBuffer() end


--windowedMode(0,0,800,600)
setBufferName("times.txt")

function demoMode()
  fullscreenMode()
  hideEditor()
  setFloorGrid(false)
  setPickSphere(false)
end

demoMode()

playMp3()
