
mouseinfo = {}

mouseinfo.clickx = function ()
  local m = mouseinfo
  if m.isdown then return m.down.x else return -m.down.x end
end

mouseinfo.clicky = function ()
  local m = mouseinfo
  if m.isdown then return m.down.y else return -m.down.y end
end

function LMBDown(x,y)
  local m = mouseinfo
  m.down = {x=x,y=y}
  m.isdown = true
  
  --showTrace()
  --print(getMp3Time())
  WidgetLib.callAllInRange("lmbdown")
end

function LMBUp(x,y)
  local m = mouseinfo
  m.isdown = false
  
  WidgetLib.callAllInRange("lmbup")
end

-- An initial version before simplifying
--[[
function LMBUp(x,y)
  local m = mouseinfo
  --m.down = {x=x,y=y}
  local negativeize = function(x)
    if x <= 0 then
      return x
    else
      return x * -1
    end
  end
  local function transformtable(t, fn)
    for k,v in pairs(t) do
      t[k] = fn(v)
    end
  end
  transformtable(m.down, negativize)
  m.isdown = false
  
  WidgetLib.callAllInRange("lmbup")
end
]]

function OnMouseMove(dx,dy,x,y)
  local m = mouseinfo
  m.pos = {x=x,y=y}
  
  WidgetLib.callAllInRange("mousemove")
end
