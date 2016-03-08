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

dofile("replacefn.lua")
dofile("editor.lua")
dofile("keymap.lua")

dofile("shiftenter.lua")
dofile("opengl-prelude.lua")

dofile("trace2.lua")
dofile("tweaks.lua")

praxis_instance_name = "praxis-empty"

do
 local clamp
 
 function clamp(x,min,max)
   if x < min then return min end
   if x > max then return max end
   return x
 end
 
 function update()
  WidgetLib.callAll("update")
  
  local r,g,b = getClearColor()
  r = clamp(r-20,0,255)
  g = clamp(g-20,0,255)
  b = clamp(b-20,0,255)
  setClearColor(r,g,b)
 end
end

function render()
  WidgetLib.callAll("render")
  
  trace2()
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

--windowedMode(50,700,250,250)


