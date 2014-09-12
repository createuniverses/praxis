points = {}
function render()
  local h = 10
  local tend = math.pi * 2
  local e = 25
  local R = 75
  
  local x = e * math.cos(3 * 0) + R * math.cos(0)
  local y = e * math.sin(3 * 0) + R * math.sin(0)
  local dt = math.pi / 180
  for t = dt,tend,dt do
    local x2 = e * math.cos(3 * t) + R * math.cos(t)
    local y2 = e * math.sin(3 * t) + R * math.sin(t)

    drawLine(x,h,y,x2,h,y2)
    x = x2
    y = y2
  end
end

function epitroch(e,R,n,t)
  local x = e * math.cos(3 * t) + R * math.cos(t + (2 * n * math.pi) / 3)
  local y = e * math.sin(3 * t) + R * math.sin(t + 2 * n * math.pi)
  return { x = x, y = y }
end

function render()
  local h = 10
  local tend = math.pi * 2
  local e = 20
  local R = 60
  local n = 0
  
  local p1 = epitroch(e,R,n,0)
  local dt = math.pi / 180
  for t = dt,tend,dt do
    local p2 = epitroch(e,R,n,t)

    drawLine(p1.x,h,p1.y,
             p2.x,h,p2.y)
             
    p1.x = p2.x
    p1.y = p2.y
  end
end

clearError()
clearTrace()
continue()

--lookDown()

