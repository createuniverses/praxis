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
  spirowidget.cogs[2] = { radius = 5, angle = 0, speed = 4 }
  spirowidget.cogs[3] = { radius = 7, angle = 0, speed = 7 }
  --spirowidget.cogs[4] = { radius = 9, angle = 0, speed = 4 }
  --spirowidget.cogs[5] = { radius = 11, angle = 0, speed = 2.5 }
  --spirowidget.cogs[6] = { radius = 4.3, angle = 0, speed = 3.7 }
  --spirowidget.cogs[7] = { radius = 8, angle = 0, speed = 2.3 }
  streamer = Queue.new()
end

windowedMode()
fullscreenMode()

do
  spirowidget.cogs[1].speed = 1
  spirowidget.cogs[2].speed = 4
  spirowidget.cogs[3].speed = 7
  streamer = Queue.new()
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
    if i==#cogs then
      addPointToStreamer(streamer, vec3d(p2.x, p2.z, p2.y))
    end
  end

  renderStreamer(streamer)
  
  for i=1,#cogs,1 do
    cogs[i].angle = cogs[i].angle + ((cogs[i].speed / 180) * 3.14159)
  end
  setmetatable(_G, nil)
end

continue()
clearError()

switchToBuffer("queue.lua")

streamer = Queue.new()
function addPointToStreamer(s, p)
  Queue.pushfirst(s, p)
  if Queue.size(s) > 1000 then
    Queue.poplast(s)
  end
end

print2(getFunction(glVector))

function glVec(v)
  vectorGL(v.x, v.y, v.z)
end

function renderStreamer(s)
  beginLinGL()
  colorGL(255,255,255,255)
  for i=1,Queue.size(s)-1,1 do
    local p1 = Queue.get(s,i)
    local p2 = Queue.get(s,i+1)
    glVec(p1)
    glVec(p2)
  end
  if Queue.size(s) > 50 then
    local i = 40
    local p1 = Queue.get(s,i)
    local p2 = Queue.get(s,i+5)
    local p3 = p1 * 0.8
    local p4 = p2 * 0.8
    glVec(p1)
    glVec(p3)
    glVec(p2)
    glVec(p4)
    glVec(p3)
    glVec(p4)
  end
  endGL()
end


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
