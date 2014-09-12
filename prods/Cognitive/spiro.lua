function drawCircle(r,cx,cy)
  local x = cx + math.sin(0) * r
  local y = cy + math.cos(0) * r
  local h = 10
  for i = 1,360,1 do
    local x2 = cx + math.sin((i/180) * math.pi) * r
    local y2 = cy + math.cos((i/180) * math.pi) * r
    drawLine(x,h,y,x2,h,y2)
    x = x2
    y = y2
  end
end

points = {}

t = 0
function render()
  local h = 10
  local r1 = 75
  local r2 = 25
  local c2p = 1
  local numites = 1

  local t1 = 0
  local c2x = 0
  local c2y = 0

  
  local dt = math.pi / 180

  local circum1 = 2 * math.pi * r1
  local circum2 = 2 * math.pi * r2

  local d1 = (circum1 / (math.pi * 2)) * dt

  local point = {x = 0, y = 0}
  
  for ites = 1,numites,1 do
    t1 = t * math.pi / 180
    c2x = math.sin(t1) * (r1+r2)
    c2y = math.cos(t1) * (r1+r2)
    
    local t2 = (d1 / circum2) * (math.pi * 2) * t
    
    point = {x = c2x + math.sin(t2) * r2 * c2p, y = c2y + math.cos(t2) * r2 * c2p}
  
    table.insert(points, point)
  
    t = t + 1
  end
  
  drawCircle(r1,0,0)
  drawCircle(r2, c2x, c2y)
  
  drawLine(c2x,     h, c2y,
           point.x, h, point.y)
  
  local px = points[1].x
  local py = points[1].y

  for i=2,#points,1 do
    local px2 = points[i].x
    local py2 = points[i].y
    drawLine(px,h,py,px2,h,py2)
    px = px2
    py = py2
  end
  
end

clearError()
clearTrace()
continue()

