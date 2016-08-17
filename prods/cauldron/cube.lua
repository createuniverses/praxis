

Cube = {}

function Cube.renderFace()
  -- grid shader
  -- checkerboard shader
  glBegin(GL_QUADS)
    glVertex(0,0,0)
    glVertex(10,0,0)
    glVertex(10,0,10)
    glVertex(0,0,10)
  glEnd()
end

function Cube.new()
 c = {}
 setmetatable(c, {__index = Cube})
 c.t = {}

 do
  local t = transform.new()
  t:setTranslation(0,0,10)
  t:setRotation(math.pi*0.25,1,0,0)
  c.t[1] = t
 end

 do
  local t = transform.new()
  t:setTranslation(0,0,10)
  c.t[2] = t
 end

 do
  local t = transform.new()
  t:setTranslation(10,0,0)
  c.t[3] = t
 end

 do
  local t = transform.new()
  t:setTranslation(0,0,10)
  t:setRotation(math.pi * 1.0, 0,1,0)
  c.t[4] = t
 end

 do
  local t = transform.new()
  t:setTranslation(10,0,0)
  t:setRotation(math.pi * 1.0, 0,1,0)
  c.t[5] = t
 end
 
 c.angle = 0
 c.angledelta = math.pi * 0.01
 
 return c
end

function Cube.update(c)
  c.angle = c.angle + c.angledelta
  
  if c.angle > math.pi * 0.5 then
    c.angle = math.pi * 0.5
    if c.auto then
      c.angledelta = -math.pi * 0.01
    end
  end
  
  if c.angle < 0.0 then
    c.angle = 0
    if c.auto then
      c.angledelta = math.pi * 0.01
    end
  end
end

function Cube.openbox(c)
  c.angledelta = math.pi * -0.01
end

function Cube.closebox(c)
  c.angledelta = math.pi * 0.01
end

cubes = {}

for i=1,10,1 do
  cubes[i] = Cube.new()
  for j=1,i*5,1 do
    cubes[i]:update()
  end
end

for i=1,10,1 do
  cubes[i]:openbox()
  --cubes[i]:closebox()
end

for i=1,10,1 do
  --cubes[i]:openbox()
  cubes[i]:closebox()
end

cubes[3].auto = false
cubes[3].auto = true

do
  local t = transform.new()
  --t:setTranslation(10,0,0)
  --t:setRotation(math.pi * 1.0, 0,1,0)
  cubes[3].t[6] = t
end

do
  local c = cubes[3]
  local t = c.t[6]
  
  --t:setTranslation(0,0,0)
  t:setRotation(c.angle, 0,1,0)
end

function Cube.updatematrices(c)
  c.t[1]:setRotation(c.angle,-1,0,0)
  c.t[2]:setRotation(c.angle,-1,0,0)
  c.t[3]:setRotation(c.angle, 0,0,1)
  c.t[4]:setRotation(math.pi * 1.0, 0,1,0)
  c.t[4]:rotate(c.angle, 0,0,-1)

  c.t[5]:setRotation(math.pi * 1.0, 0,1,0)
  c.t[5]:rotate(c.angle, 1,0,0)
end

function Cube.render(c)
  enablePolygonOffset()
  setPolygonOffset(-1,1)

  -- render base face
  -- apply transform, render and unapply transform for 1,2 and 3
  -- render neighbours 1,2 and 3
  -- render neighbour 4
  -- render neighbour 4's neighbour.

  glPushMatrix()
    if c.t[6] ~= nil then
    c.t[6]:setRotation(c.angle*2, 1,0,0)
    --c.t[6]:setTranslation(0,math.cos(c.angle*2) * 10,math.sin(c.angle*2) * 10+10)
    --glApplyTransform(c.t[6])
    end

  glColor(255,255,0,255)
  Cube.renderFace()

  glPushMatrix()
    glApplyTransform(c.t[1])
    glColor(255,0,0,255)
    Cube.renderFace()

    glApplyTransform(c.t[2])
    glColor(255,0,255,255)
    Cube.renderFace()
  glPopMatrix()

  glPushMatrix()
    glApplyTransform(c.t[3])
    glColor(155,0,0,255)
    Cube.renderFace()
  glPopMatrix()

  glPushMatrix()
    glApplyTransform(c.t[4])
    glColor(55,0,255,255)
    Cube.renderFace()
  glPopMatrix()

  glPushMatrix()
    glApplyTransform(c.t[5])
    glColor(55,255,0,255)
    Cube.renderFace()
  glPopMatrix()

  glPopMatrix()
  
  disablePolygonOffset()
end

function transformtest(x,y,z)
  local cid = 3
  local tid = 5
  return cubes[cid].t[tid]:transformPoint(x,y,z)
end

do
  local c = cubes[3]
  local t = transform.new()
  c.t[7] = t
  t:copy(c.t[1])
  t:transform(c.t[2])
  t:invert()
end

function transformtest(x,y,z)
  local t = cubes[3].t[7]
  x,y,z = t:transformPoint(x,y,z)
  return x,y,z

  --local cid = 3
  --x,y,z = cubes[cid].t[1]:transformPointInverse(x,y,z)
  --x,y,z = cubes[cid].t[2]:transformPointInverse(x,y,z)
  --return x,y,z
end

function renderlocalled()
  glPushMatrix()
  glTranslate(0,1,0)
  glBegin(GL_LINES)
  glVertex(transformtest(0,0,0))
  glVertex(transformtest(10,0,0))

  glVertex(transformtest(10,0,0))
  glVertex(transformtest(10,0,10))

  glVertex(transformtest(10,0,10))
  glVertex(transformtest(0,0,10))

  glVertex(transformtest(0,0,10))
  glVertex(transformtest(0,0,0))
  glEnd()
  glPopMatrix()
end


function render()
  WidgetLib.renderAll()

  glPushMatrix()
  for i=1,10,1 do
    cubes[i]:update()
    cubes[i]:update()
    cubes[i]:update()
    if i == 3 then
    else
    cubes[i]:render()
    end
    glTranslate(35,0,0)
  end
  glPopMatrix()

  renderlocalled()


  --renderGreets2()
  --renderskythings()
  trace2()
end

function drawCurve(p1,p2)
  local pa = calcCurve(p1,p2,0)
  for t=0.01,1,0.01 do
    local pb = calcCurve(p1,p2,t)
    drawLine(pa.x, pa.y, pa.z, pb.x, pb.y, pb.z)
    pa = pb
  end
end

curve = {}
curve.p1 = {p = vec3d(0,0,0), t = vec3d(0,50,0)}
curve.p2 = {p = vec3d(10,40,10), t = vec3d(0,50,0)}

plant = {}
plant.stem = curve
plant.leaf = {}
plant.leaf.top = {}
plant.leaf.top.p1 = {p = calcCurve(curve.p1, curve.p2, 0.6), t = vec3d(0,-30,40)}
plant.leaf.top.p2 = {p = calcCurve(curve.p1, curve.p2, 0.6) + vec3d(0,0,40), t = vec3d(0,-20,40)}

plant.leaf.bottom = {}
plant.leaf.bottom.p1 = {p = calcCurve(curve.p1, curve.p2, 0.6), t = vec3d(0,30,40)}
plant.leaf.bottom.p2 = {p = calcCurve(curve.p1, curve.p2, 0.6) + vec3d(0,0,40), t = vec3d(0,-20,40)}

oldt = transform.new()

--cubes[3].auto = true

showFPS()

do

 cubes[3].lerpt = 0
 cubes[3].auto = false
 cubes[3]:openbox()

 function updatecoobmat()
  local c = cubes[3]
  local t = c.t[7]
  oldt:copy(t)
  t:copy(c.t[4])
  --t:transform(c.t[2])
  t:invert()
  t:interpolate(oldt, t, c.lerpt)
 end
end

do updatefns["goup"] = function ()
  cubes[3].lerpt = cubes[3].lerpt + 0.02
  if cubes[3].lerpt > 1.0 then
    cubes[3].lerpt = 1.0
    cubes[3].auto = true
  end
end end

function render()
  WidgetLib.renderAll()

  for i=1,10,1 do
    cubes[i]:update()
    cubes[i]:update()
    cubes[i]:update()
    cubes[i]:updatematrices()
    glPushMatrix()
    glTranslate(60 * math.sin(i*math.pi * 0.2),0,60 * math.cos(i*math.pi * 0.2))
    if i == 3 then
      updatecoobmat()
      glApplyTransform(cubes[i].t[7])
    end
    cubes[i]:render()
    glPopMatrix()
  end

  drawCurve(curve.p1, curve.p2)
  drawCurve(plant.leaf.top.p1, plant.leaf.top.p2)
  drawCurve(plant.leaf.bottom.p1, plant.leaf.bottom.p2)

    glPushMatrix()
    glTranslate(5,40,5)
    cubes[1]:render()
    glPopMatrix()

  --orbitCamPP(5,30,5, math.pi * 0.01, 0)

  renderlocalled()

  --renderGreets2()
  --renderskythings()
  trace2()
end


cubes[1]:openbox()


boxt = 0
cubes[2].auto = not cubes[2].auto

for i=1,10,1 do
  cubes[i]:closebox()
  cubes[i].auto = false
end


function openboxes()
  boxt = boxt + 1
  for i=1,10,1 do
    if boxt == i * 3 then
      --cubes[i]:closebox()
      --cubes[i]:openbox()
      cubes[i].auto = not cubes[i].auto
    end
  end
end

updatefns["openboxes"] = openboxes
