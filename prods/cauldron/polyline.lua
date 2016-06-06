Widgets["poly"] = WidgetLib2.new()

polyw = Widgets["poly"]

polyw.polyline = {}

function drawpt(p)
  glColor(255,0,0,255)
  drawLine(p.x, 5, p.y - 5,
           p.x, 5, p.y + 5)
  drawLine(p.x - 5, 5, p.y,
           p.x + 5, 5, p.y)
end

function drawpolyline(p,drawpointoption,r,g,b)
  for i=1,numsegments(p),1 do
    local seg = getsegment(p,i)
    if drawpointoption then
      drawpt(seg.a)
      drawpt(seg.b)
    end
    glColor(r,g,b,255)
    drawLine(seg.a.x, 5, seg.a.y,
             seg.b.x, 5, seg.b.y)
  end
end

function polyw:render()
  local p = self.polyline
  local p2 = self.rpolyline

  glPushMatrix()
  --glTranslate(-300,0,-300)

  --glColor(255,255,255,255)
  drawpolyline(p,false,255,255,255)

  --glColor(0,255,255,255)
  drawpolyline(p2, true,0,255,255)


  if pos ~= nil then
    drawpt(polylinepostorealpos(p, pos))
  end
  
  glPopMatrix()
end

function makepolyline(numL)
  local polyline = {}
  direction = 0
  lineStepSize = 5
  polyline[1] = { x = 0, y = 0 }
  for i = 1, numL, 1 do
    polyline[i+1] = 
    {
      x = polyline[i].x + (lineStepSize + math.random(lineStepSize) - (lineStepSize * 0.5)) * ( math.sin(direction * (math.pi / 180))),
      y = polyline[i].y + (lineStepSize + math.random(lineStepSize) - (lineStepSize * 0.5)) * ( math.cos(direction * (math.pi / 180)))
    }
    --direction = direction + (45 + math.random(10) - 5) * (math.random(2) - 1)
    direction = direction + math.random(90) - 45
  end
  return polyline
end

function distance(p1, p2)
  local diff = {x = p2.x - p1.x, y = p2.y - p1.y }
  return math.sqrt(diff.x * diff.x + diff.y * diff.y)
end

function segmentlength(s)
  return distance(s.a, s.b)
end

function numsegments(polyline)
  return #polyline - 1
end

function getsegment(polyline, id)
  if id < #polyline then
    return {a = polyline[id], b = polyline[id+1] }
  else
    return nil
  end
end

function computepolylinepos(polyline, distance)
  local searching = true
  local id = 1
  local walklen = 0
  while searching do
    local segment = getsegment(polyline, id)

    if segment == nil then
      searching = false
      do return nil end
    end
    
    local seglen = segmentlength(segment)
    local walklen2 = walklen + seglen
    if walklen2 > distance then
      local result = { segid = id, p = (distance - walklen) / seglen }
      return result
    end
    
    walklen = walklen2
    id = id + 1
  end
  
  return nil
end

pos = computepolylinepos(polyw.polyline, 180)

function interpolatesegment(segment, p)
  local x = segment.a.x + (segment.b.x - segment.a.x) * p
  local y = segment.a.y + (segment.b.y - segment.a.y) * p
  return {x = x, y = y}
end

function polylinepostorealpos(polyline, pos)
  local segment = getsegment(polyline, pos.segid)
  if segment == nil then
    return {x = 0, y = 0}
  end
  return interpolatesegment(segment, pos.p)
end

function resamplepolyline(polyline, distance)
  local stepping = true
  local pos = 0
  local rpolyline = {}
  while stepping do
    local polypos = computepolylinepos(polyline, pos)
    if polypos == nil then return rpolyline end
    local realpos = polylinepostorealpos(polyline, polypos)
    if realpos == nil then return rpolyline end
    table.insert(rpolyline, realpos)
    pos = pos + distance
  end
  return rpolyline
end

function makecoldseekerpolyline()
  local p = {}
  local points = {300, 300, 420, 400, 500, 450, 580, 400, 700, 300}
  for i=1,#points,2 do
    table.insert(p, {x = points[i], y = points[i+1]})
  end
  return p
end

--polyw.polyline = makecoldseekerpolyline()
--polyw.rpolyline = resamplepolyline(polyw.polyline, 82.093)

polyw.polyline = makepolyline(40)
polyw.rpolyline = resamplepolyline(polyw.polyline, 16)


function setCamPosXY(x,y)
  local curpos = {}
  curpos.x, curpos.y, curpos.z = getCamPos()
  setCamPos(x, curpos.y, y)
end

setCamPosXY(0,0)

--clearError()
--clearTrace()
continue()

lookDown()

--print2(getErrorText())
--print2(getFunction(lookDown))
function lookDown()
  local pos = { getCamPos() }
  -- pos[1] = pos[1] + 10 -- look along x
  pos[3] = pos[3] + 10 -- look along z
  lookAt(table.unpack(pos))
  rotateCam(0, math.pi * -0.5)
end

