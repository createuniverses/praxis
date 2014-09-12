
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

function initRobots()
  robots = {}
  for i=1,150,1 do
    addRobot(0,0)
    --addRobot(math.random(20),math.random(20))
  end
end

-- should add this as a widget

initRobots()

robotWidget = {}

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

WidgetLib.addExisting(robotWidget)

function moveRobotWidget()
  --robotWidget.lspace = transform.new()
  transform.setTranslation(robotWidget.lspace, -100,0,-200)
  transform.lookAt(robotWidget.lspace, 0,100,0)
end
