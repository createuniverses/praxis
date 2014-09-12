-- Name: drawing.lua

function shade(f,col)
  col.r = col.r * f
  col.g = col.g * f
  col.b = col.b * f
  if col.r < 0 then col.r = 0 end
  if col.g < 0 then col.g = 0 end
  if col.b < 0 then col.b = 0 end
  if col.r > 255 then col.r = 255 end
  if col.g > 255 then col.g = 255 end
  if col.b > 255 then col.b = 255 end
  colorGL(col.r, col.g, col.b, col.a)
end

function drawTriangleGL(p1,p2,p3)
  local norm = Vector3D.cross(p2-p1, p3-p1)
  norm = Vector3D.normalize(norm)
  normalGL(norm.x, norm.y, norm.z)
  vectorGL(p1.x, p1.y, p1.z)
  normalGL(norm.x, norm.y, norm.z)
  vectorGL(p2.x, p2.y, p2.z)
  normalGL(norm.x, norm.y, norm.z)
  vectorGL(p3.x, p3.y, p3.z)
end

function drawPolygon(pos,r,n,s,h)
  local currCol = { }
  currCol.r,currCol.g,currCol.b,currCol.a = getColorGL()
  
  beginTriGL()
  
  angstep = (math.pi * 2) / n
  for i=s,math.pi * 2+s,angstep do
    shade(0.95, currCol)
    
    local p1 = vec2d(r*math.cos(i        ), r*math.sin(i        ))
    local p2 = vec2d(r*math.cos(i+angstep), r*math.sin(i+angstep))
    
    local pos2 = pos + vec3d(0,h,0)
    
    drawTriangleGL(pos, pos+p1, pos+p2)
    drawTriangleGL(pos2, pos2+p2, pos2+p1)
    
    -- now draw the connecting quad
    
    drawTriangleGL(pos+p2, pos+p1, pos2+p1)
    drawTriangleGL(pos2+p1, pos2+p2, pos+p2)
  end
  
  endGL()
end

function drawCurve(p1, p2)
  --disableLighting()
  colorGL(0,255,0,255)
  beginLinGL()
  for t=0,0.801,0.01 do
    local pos1 = calcCurve(p1,p2,t)
    local pos2 = calcCurve(p1,p2,t+0.2)
    --local pos2 = calcCurve(p1,p2,t+0.01)
    normalGL(0,1,0)
    vectorGL(pos1.x, pos1.y, pos1.z)
    normalGL(0,1,0)
    vectorGL(pos2.x, pos2.y, pos2.z)
    --drawLine(pos1.x, pos1.y, pos1.z,
    --         pos2.x, pos2.y, pos2.z)
  end
  endGL()
  --enableLighting()
end

function drawTexQuad(pos,s)
  enableTexturing()
  
  beginQuadGL()
  
  texGL(0,0)
  normalGL(0,1,0)
  vectorGL(pos.x,   pos.y, pos.z)
  
  texGL(1,0)
  normalGL(0,1,0)
  vectorGL(pos.x,   pos.y, pos.z+s)
  
  texGL(1,1)
  normalGL(0,1,0)
  vectorGL(pos.x+s, pos.y, pos.z+s)
  
  texGL(0,1)
  normalGL(0,1,0)
  vectorGL(pos.x+s, pos.y, pos.z)
  
  endGL()
  
  disableTexturing()
end

function drawEmptyPolygon(pos,r,n,s)
  angstep = (math.pi * 2) / n
  for i=s,math.pi * 2+s,angstep do
    drawLine(
      pos.x+r*math.cos(i),         pos.y, pos.z+r*math.sin(i),
      pos.x+r*math.cos(i+angstep), pos.y, pos.z+r*math.sin(i+angstep))
  end
end

function drawCircle(pos,r,h)
  drawPolygon(pos,r,20,0,h)
end

function drawEmptyCircle(pos,r)
  drawEmptyPolygon(pos,r,20,0)
end

function drawSquare(pos,r,h)
  drawPolygon(pos,r,4,math.pi/4,h)
end

function drawTriangle(pos,r,h)
  drawPolygon(pos,r,3,0,h)
end

function drawArrow(p1, p2, h)
  p1 = p1 + vec3d(0,h,0)
  p2 = p2 + vec3d(0,h,0)
  drawLine(p1.x, p1.y, p1.z,
           p2.x, p2.y, p2.z)
  
  local direction = Vector3D.normalize(p2 - p1) * 2
  local perp_direction = Vector3D.normalize(Vector3D.ortho2D(direction)) * 2
  local arrow_base = p2 - direction
  local arrow_left = arrow_base - perp_direction
  local arrow_right = arrow_base + perp_direction
  
  drawLine(p2.x,           p2.y,           p2.z,
           arrow_left.x,   arrow_left.y,   arrow_left.z)
  drawLine(p2.x,           p2.y,           p2.z,
           arrow_right.x,  arrow_right.y,  arrow_right.z)
end

function drawBond(p1, p2, h)
end
