-- widget testing

editfn("Widgets[2].render")

function WidgetLib.addRender(r)
  local w = {}
  w.lspace = transform.new()
  w.width = 10
  w.height = 10
  w.depth = 10
  w.anchored = false  -- I forget the meaning of this
  w.render = r
  w.update = function (o) end
  w.lmbdown = function (o,x,y,z) end
  w.lmbup = function (o,x,y,z) end
  w.rmbdown = function (o,x,y,z) end
  w.rmbup = function (o,x,y,z) end
  w.mousemove = function (o,x,y,z) end
  table.insert(Widgets, w)
  return w
end

lineWidget = {}
function lineWidget.render()
  drawLine(-100,0,-200, 0,100,0)
end

WidgetLib.addExisting(lineWidget)

-- should add a widget adding function that takes a render function
-- produces a new widget with it and returns that widget so its
-- render function can be changed.

robots = {}

function addRobot(x,y)
  local r = {}
  r.x = x
  r.y = y
  r.dx = math.random(20) + 5
  r.dy = math.random(20)
  table.insert(robots, r)
  return r
end

function updateRobot(r)
  r.x = r.x + r.dx
  r.y = r.y + r.dy
  r.dx = r.dx - 1
  r.dy = r.dy - 1
  if r.dx < 0 then r.dx = 0 end
  if r.dy < 0 then r.dy = 0 end
end

function renderRobot(r)
  colorGL(255,255,255,255)
  local h = 20
  local s = 10
  drawLine(r.x - s, h, r.y,
           r.x + s, h, r.y)
  drawLine(r.x    , h, r.y - s,
           r.x    , h, r.y + s)
end

for i=1,150,1 do
  addRobot(0,0)
  --addRobot(math.random(20),math.random(20))
end

-- should add this as a widget

--robotWidget.lspace = transform.new()

transform.setTranslation(robotWidget.lspace, -100,0,-200)
transform.lookAt(robotWidget.lspace, 0,100,0)

robotWidget = WidgetLib.newSimple()

function robotWidget.render()
  for i=1,#robots,1 do
    renderRobot(robots[i])
  end
end

function robotWidget.update()
  for i=1,#robots,1 do
    updateRobot(robots[i])
  end
end

num = 0
function render()
  
  for i = 0, 100, 5 + 3 * math.sin(num * 0.15) do
    drawLine(-100 * math.sin(num * 0.07),10,i,100 * math.sin(num * 0.13),10,i * math.sin(num * 0.09))
  end
  num = num + 1
end
