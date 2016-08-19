
function csection()
  local s = 2
  glBegin(GL_LINES)
    glVertex(0,0,0)
    glVertex(0,s,0)

    glVertex(0,s,0)
    glVertex(s,s,0)

    glVertex(s,s,0)
    glVertex(s,0,0)

    glVertex(s,0,0)
    glVertex(0,0,0)
  glEnd()
end
showFPS()

do
 local s = 2
 csectionpts = {}
 
 table.insert(csectionpts, vec3d(0,0,0))
 table.insert(csectionpts, vec3d(0,s,0))
 table.insert(csectionpts, vec3d(s,s,0))
 table.insert(csectionpts, vec3d(s,0,0))
end

do
  local r = 3
  local sides = 3
  csectionpts = {}
  local step = (math.pi * 2.01)/sides
  for a = 0,math.pi*2,step do
    --print2(a)
    local x = math.sin(a) * r
    local y = math.cos(a) * r
    local z = 0
    table.insert(csectionpts, vec3d(x,y,z))
  end
  --print2(#csectionpts)
end

function helix()
  local s = math.pi * 0.1
  local r = 20
  local v = 4.7
  local a = 0
  local x1 = r * math.sin(a)
  local y1 = r * math.cos(a)
  local z1 = a * v
  for a=0,6*math.pi,s do
    local x2 = r * math.sin(a)
    local y2 = r * math.cos(a)
    local z2 = a * v
    drawLine(x1,y1,z1, x2,y2,z2)
    x1,y1,z1 = x2,y2,z2
    
    glPushMatrix()
    glTranslate(x1,y1,z1)
    csection()
    glPopMatrix()
  end
end

helixt = transform.new()
helixt:translate(0,2,0)

function transformpoints(t, pts, npts)
  for i=1,#pts,1 do
    local pt = pts[i]
    npts[i] = vec3d(t:transformPoint(pt:getArgs()))
  end
end

do
  local t = helixt:transformPoint(vec3d(0,0,0):getArgs())
end


function drawLines(pts1, pts2)
  for i=1,#pts1, 1 do
    local p1 = pts1[i]
    local p2 = pts2[i]
    if p1 ~= nil and p2 ~= nil then
      drawLine(unpack2({p1:getArgs()}, {p2:getArgs()}))
    end
  end
end

colors = {}
table.insert(colors, {r = 200, g = 0,   b = 0})
table.insert(colors, {r = 200, g = 100, b = 0})
table.insert(colors, {r = 200, g = 100, b = 100})
table.insert(colors, {r = 100, g = 200, b = 100})
table.insert(colors, {r = 100, g = 100, b = 200})
table.insert(colors, {r = 0,   g = 100, b = 200})
table.insert(colors, {r = 0,   g = 0,   b = 200})
table.insert(colors, {r = 0,   g = 100, b = 100})
table.insert(colors, {r = 0,   g = 200, b = 0})
table.insert(colors, {r = 100, g = 100, b = 0})

curcolor = 1

function drawTriangle(p1,p2,p3)
  local col = colors[curcolor]
  local ncol = #colors
  curcolor = ((curcolor+0)%ncol) + 1
  
  glColor(col.r, col.g, col.b, 255)
  glBegin(GL_TRIANGLES)
    glVertex(p1:getArgs())
    glVertex(p2:getArgs())
    glVertex(p3:getArgs())
  glEnd()
end

function drawLines(pts1, pts2)
  local numpts = #pts1
  for i=1,numpts,1 do
    local p1 = pts1[i]
    local p2 = pts2[i]
    
    local j = i+1
    if j>numpts then j=1 end
    
    local p3 = pts1[j]
    local p4 = pts2[j]
    if p1 ~= nil and p2 ~= nil and p3 ~= nil and p4 ~= nil then
      --drawLine(unpack2({p1:getArgs()}, {p2:getArgs()}))
      drawTriangle(p1,p2,p3)
      drawTriangle(p3,p4,p2)
    end
  end
end

--print2(unpack2({vec3d(1,2,3):getArgs()}, {vec3d(4,5,6):getArgs()}))


continue()

helixnum = 0.05
dhelixnum = -0.001
do updatefns["swooshy"] = function ()
  helixnum = helixnum + dhelixnum
  if helixnum < 0.005 then
    dhelixnum = 0.002
  end
  if helixnum > 0.15 then
    dhelixnum = -0.002
  end
end end

function helixsolid()
  local r = 20
  local a = 0
  helixt = transform.identity()
  helixt:translate(0,10,0)
  --helixt:translate(r*math.sin(a), r*math.cos(a), 0)
  local ptsa = {}
  local ptsb = {}
  --local pts = ptsa
  
  for i=1,100,1 do
    transformpoints(helixt, csectionpts, ptsa)
    drawLines(ptsa, ptsb)
    ptsa,ptsb = ptsb,ptsa
    
    glPushMatrix()
    glApplyTransform(helixt)
    csection()
    glPopMatrix()

    --helixt:translate((vec3d(helixt:side())*15):getArgs())
    --helixt:rotate(math.pi * 0.1, 0,0,1)
    --helixt:translate((vec3d(helixt:side())*-15):getArgs())

    --helixt:translate(0,0,3)
     
    -- do a barrel roll
    --helixt:rotate(math.pi * 0.03, (vec3d(helixt:forward())):getArgs())
    helixt:rotate(math.pi * helixnum, (vec3d(helixt:forward())):getArgs())
    helixt:rotate(math.pi * 0.02, (vec3d(helixt:side())):getArgs())
    
    helixt:translate((vec3d(helixt:forward())*2):getArgs())

  end
end

continue()
rt = 0

function render()
  WidgetLib.renderAll()

  rt = rt + 5

  glPushMatrix()

  glRotate(rt,0,0,1)

  helix()

  glPopMatrix()

  helixsolid()

  trace2()
end







