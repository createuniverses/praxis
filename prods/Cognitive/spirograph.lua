fullscreenMode()
edSetRenderMode(0)
setBufferName("spirograph.lua")

windowedMode()
setMaxFramerate(30)

function renderSpiro()
end

spirowidget = WidgetLib.newSimple()

glColor = colorGL

function pointOnCircle(angle, radius)
  local v = vec3d(radius * math.sin(angle),
                  radius * math.cos(angle),
                  0);
  return v
end

do
  spirowidget.cogs = {}
  spirowidget.cogs[1] = { radius = 10, angle = 0, speed = 1 }
  spirowidget.cogs[2] = { radius = 10, angle = 0, speed = 1 }
  spirowidget.cogs[3] = { radius = 10, angle = 0, speed = 1 }
end

do
  spirowidget.cogs[1].speed = 1
  spirowidget.cogs[2].speed = 3
  spirowidget.cogs[3].speed = 2
end

function spirowidget.render(w)
  setmetatable(_G, { __index = function(t,k) return spirowidget[k] end } )
  glColor(190,190,10)

  local p1 = vec3d(0,0,0)
  local p2 = pointOnCircle(cogs[1].angle, cogs[1].radius)
  drawLine(p1.x,p1.z,p1.y, p2.x,p2.z,p2.y)
  for i=2,#cogs,1 do
    p1 = p2
    p2 = p2 + pointOnCircle(cogs[i].angle, cogs[i].radius)
    drawLine(p1.x,p1.z,p1.y, p2.x,p2.z,p2.y)
  end
  
  for i=1,#cogs,1 do
    cogs[i].angle = cogs[i].angle + ((cogs[i].speed / 180) * 3.14159)
  end
  setmetatable(_G, nil)
end

continue()
clearError()

print(t)
t = getmetatable(_G)
print(inspect(t))

tempt = {}
tempt.val = 5

print(inspect(tempt))

setmetatable(tempt, nil)
clearTrace()


tempmt = {}

function tempmt.__index(t,k)
  print("trying to access " .. k)
  return tempt[k]
end

print(tempt["val"])
do
  k="val"
  print(tempt[k])
end

setmetatable(_G, tempmt)
setmetatable(_G, nil)

clearError()
continue()

do
  setmetatable(_G, tempmt)
  print(val)
  setmetatable(_G, nil)
end

windowedMode()
fullscreenMode()
