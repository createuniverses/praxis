setBufferName("arch.lua")

airplane = airplane or WidgetLib.newSimple()

do
controls =
{
  pitchup = false,
  pitchdown = false,
  bankleft = false,
  bankright = false,
  yawleft = false,
  yawright = false,
  thrust = 0
}
end

if platform() == "windows" then
  stdkeyids["w"] = 87
  stdkeyids["a"] = 65
  stdkeyids["s"] = 83
  stdkeyids["d"] = 68
  stdkeyids["e"] = 69
  stdkeyids["q"] = 81
  
  stdkeyids["r"] = 82
  stdkeyids["f"] = 70
end

if platform() == "linux" then
  stdkeyids["a"] = 38
  stdkeyids["s"] = 39
  stdkeyids["d"] = 40
  stdkeyids["f"] = 41
  stdkeyids["q"] = 24
  stdkeyids["w"] = 25
  stdkeyids["e"] = 26
  stdkeyids["r"] = 27
end


do
setKeyHandlerProgram(keymap2, stdkeyids["w"], 0, "controls.pitchup = true")
setKeyHandlerProgram(keymap2, stdkeyids["a"], 0, "controls.bankleft = true")
setKeyHandlerProgram(keymap2, stdkeyids["s"], 0, "controls.pitchdown = true")
setKeyHandlerProgram(keymap2, stdkeyids["d"], 0, "controls.bankright = true")
setKeyHandlerProgram(keymap2, stdkeyids["q"], 0, "controls.yawleft = true")
setKeyHandlerProgram(keymap2, stdkeyids["e"], 0, "controls.yawright = true")

setKeyHandlerProgram(keymap2, stdkeyids["r"], 0, "controls.thrust = controls.thrust + 0.5")
setKeyHandlerProgram(keymap2, stdkeyids["f"], 0, "controls.thrust = controls.thrust - 0.5")

setKeyHandlerProgram(keymap2up, stdkeyids["w"], 0, "controls.pitchup = false")
setKeyHandlerProgram(keymap2up, stdkeyids["a"], 0, "controls.bankleft = false")
setKeyHandlerProgram(keymap2up, stdkeyids["s"], 0, "controls.pitchdown = false")
setKeyHandlerProgram(keymap2up, stdkeyids["d"], 0, "controls.bankright = false")
setKeyHandlerProgram(keymap2up, stdkeyids["q"], 0, "controls.yawleft = false")
setKeyHandlerProgram(keymap2up, stdkeyids["e"], 0, "controls.yawright = false")
end

do
  airplane.speed = 0
  
  airplane.update = function (o)
    local side = vec3d(transform.side(o.lspace))
    local forward = vec3d(transform.forward(o.lspace))
    local up = vec3d(transform.up(o.lspace))

    if controls.pitchup then
      transform.rotate(o.lspace, 5 * (math.pi/180),side.x,side.y,side.z)
    end
    if controls.pitchdown then
      transform.rotate(o.lspace, -5 * (math.pi/180),side.x,side.y,side.z)
    end
    if controls.bankleft then
      transform.rotate(o.lspace, 5 * (math.pi/180),forward.x, forward.y, forward.z)
    end
    if controls.bankright then
      transform.rotate(o.lspace, -5 * (math.pi/180),forward.x, forward.y, forward.z)
    end
    if controls.yawleft then
      transform.rotate(o.lspace, -5 * (math.pi/180),up.x, up.y, up.z)
    end
    if controls.yawright then
      transform.rotate(o.lspace, 5 * (math.pi/180),up.x, up.y, up.z)
    end
    --if controls.thrust then
    --  transform.translate(o.lspace, forward.x, forward.y, forward.z)
    --end
    local vel = forward * o.speed
    transform.translate(o.lspace, vel.x, vel.y, vel.z)

    side = vec3d(transform.side(o.lspace))
    forward = vec3d(transform.forward(o.lspace))
    up = vec3d(transform.up(o.lspace))

    if false then
      --transform.copy(transform.cameraBase(), o.lspace)
      transform.copy(transform.camera(), o.lspace)

      local offset = up * 5 + side * 0
  
      transform.translate(transform.camera(), offset.x, offset.y, offset.z)
    end
    
    o.speed = o.speed + (controls.thrust - o.speed) * 0.1
    
    transform.normalise(o.lspace)
    transform.normalise(transform.camera())
  end
  
  airplane.render = function (o)
  
  end
end

transform.copy(Widgets[2].lspace, transform.identity())
--transform.copy(transform.cameraBase(), transform.identity())
--transform.copy(transform.camera(), transform.identity())

continue()
clearError()
clearTrace()

--print2(getFunction(Widgets[2].render))

do
  planemodel = {}
  function deg2rad(a)
    return math.pi * (a/180)
  end
  function rad2deg(a)
    return 180 * (a/math.pi)
  end
  local points = {}
  for a = -90,90,10 do
    local p = vec3d(math.sin(deg2rad(a)), math.cos(deg2rad(a)), 0)
    p = p * 10
    table.insert(points, p)
  end
  
  local displacement = vec3d(0,0,1)
  --local scale = vec3d((math.random(60) + 70) * 0.01, (math.random(60) + 70) * 0.01, 1)
  local scale = vec3d(0.99, 1.01, 1)
  
  local points2 = {}
  
  for i = 1,#points,1 do
    local p1 = points[i]
    local p2 = p1 + displacement
    p2 = p2 * scale
    table.insert(points2, p2)
  end
  
  for i = 1,#points - 1,1 do
    local p1 = points[i]
    --local p2 = p1 + displacement
    local p2 = points2[i]
    local p3 = points[i+1]
    --local p4 = p3 + displacement
    local p4 = points2[i+1]
    --table.insert(points2, p2)
    table.insert(planemodel, {p1, p2, p3, p4})
  end
  
  --print("wotwot")
  
  for i=1,72,1 do
    --local r = 5
    --displacement = vec3d(r*math.sin(deg2rad(i*10)), 0, r*math.cos(deg2rad(i*10)))
    points = points2
    points2 = {}
    
    --local scale = vec3d((math.random(60) + 70) * 0.01, (math.random(60) + 70) * 0.01, 1)
    --local scale = vec3d(0.99, 1.01, 1)
    for i = 1,#points,1 do
      local p1 = points[i]
      local p2 = p1 + displacement
      --p2 = p2 * vec3d(0.9, 1.3, 1)
      p2 = p2 * scale
      table.insert(points2, p2)
    end
    
    for i = 1,#points - 1,1 do
      local p1 = points[i]
      --local p2 = p1 + displacement
      local p2 = points2[i]
      local p3 = points[i+1]
      --local p4 = p3 + displacement
      local p4 = points2[i+1]
      --table.insert(points2, p2)
      table.insert(planemodel, {p1, p2, p3, p4})
    end
  end

  renderModelIdx = 1
  
  function renderModel(m)
  glBeginQuads()
    for j=1,180,1 do
      local i = ((renderModelIdx + j) % #m) + 1
      if math.random(100) < 120 then
      colorGL(50,20,205,255)
      vectorGL(m[i][1].x, m[i][1].y, m[i][1].z)
      colorGL(100,50,200,255)
      vectorGL(m[i][2].x, m[i][2].y, m[i][2].z)
      colorGL(100,50,200,255)
      vectorGL(m[i][4].x, m[i][4].y, m[i][4].z)
      colorGL(40,30,220,255)
      vectorGL(m[i][3].x, m[i][3].y, m[i][3].z)
      end
    end
  glEnd()
  renderModelIdx = renderModelIdx + 1
  end
  
  function quadToTransform(q)
    local t = transform.new()
    transform.setTranslation(t, q[1].x, q[1].y, q[1].z)
    
    local p1 = vec3d(0,0,0)
    local p2 = q[2] - q[1]
    local p3 = q[3] - q[1]
    local p4 = q[4] - q[1]
    
    if false then
      q[1] = p1
      q[2] = p2
      q[3] = p3
      q[4] = p4
    else
      local d2 = Vector3D.magnitude(p2)
      local d3 = Vector3D.magnitude(p3)
      local d4 = Vector3D.magnitude(p4)
      
      local up = Vector3D.cross(p2, p3)
      transform.lookAt(t, q[2].x, q[2].y, q[2].z, up.x, up.y, up.z)
      
      q[1] = vec3d(0,0,0)
      q[2] = vec3d(0,0,d2)
      q[3] = vec3d(d3,0,0)
      q[4] = vec3d(d3,0,d2)
    end
    
    return t
  end
  
  function renderModel(m)
    for i=1,#m,1 do
      if m[i].t == nil then
        m[i].t = quadToTransform(m[i])
      end
      
      glPushMatrix()
      glApplyTransform(m[i].t)
      
      glBeginQuads()
      colorGL(250,20,205,255)
      vectorGL(m[i][1].x, m[i][1].y, m[i][1].z)
      colorGL(100,50,200,255)
      vectorGL(m[i][2].x, m[i][2].y, m[i][2].z)
      colorGL(100,50,200,255)
      vectorGL(m[i][4].x, m[i][4].y, m[i][4].z)
      colorGL(40,30,220,255)
      vectorGL(m[i][3].x, m[i][3].y, m[i][3].z)
      glEnd()
      
      glPopMatrix()
      
      transform.rotate(m[i].t, 0, deg2rad(2 + i*0.01))
      transform.normalise(m[i].t)
      --transform.rotate(m[i].t, deg2rad(2),0)
    end
  end
end

do
  fanAroundPoint = function (c, d, r, m)
    local model = {}
    local points = {}
    local c2 = c - vec3d(0,d,0)
    for a = 0,360,10 do
      local a = deg2rad(a)
      local p = c2 + (vec3d(math.sin(a), 0, math.cos(a)) * r)
      table.insert(points, p)
    end
    for i=1,#points-1,1 do
      table.insert(m, { c, points[i], points[i+1] })
    end
    return points
  end
  
  function extrude(pts, d, r, m)
  end
  
  model2 = {}
  local mpts = fanAroundPoint(vec3d(0,100,0), 20, 10, model2)
  
  function renderTriModel(m)
    glBeginTriangles()
      for i=1,#m,1 do
        colorGL(250,20,205,255)
        vectorGL(m[i][1].x, m[i][1].y, m[i][1].z)
        colorGL(10,50,200,255)
        vectorGL(m[i][2].x, m[i][2].y, m[i][2].z)
        colorGL(100,50,200,255)
        vectorGL(m[i][3].x, m[i][3].y, m[i][3].z)
      end
    glEnd()
  end
end

function projectPoint(p,t)
  local pp = nil
  local inside = false
  return pp,inside
end

function renderTextUsingStrokeChar(s)
  -- using StrokeChar
end

function projectPointToLine(p,la,lb)
  local f = lb - la
  local fn = Vector3D.normalize(f);
  local dp = Vector3D.dot(fn, p-la)
  return la + (fn * dp)
end

function gentesttri()
  testtriangle = 
  {
    vec3d(0,0,0),
    vec3d(math.random(60) - 30,0,math.random(60) - 30),
    vec3d(math.random(60) - 30,0,math.random(60) - 30)
  }
end

gentesttri()

function renderInsideOutside(o)
  local t = 
  {
    cvec3d(testtriangle[1]),
    cvec3d(testtriangle[2]),
    cvec3d(testtriangle[3])
  }
  
  t[1] = t[1] + vec3d(100,0,100)
  t[2] = t[2] + vec3d(100,0,100)
  t[3] = t[3] + vec3d(100,0,100)
  
  local t2 = 
  {
    projectPointToLine(t[1], t[2], t[3]),
    projectPointToLine(t[2], t[1], t[3]),
    projectPointToLine(t[3], t[1], t[2])
  }
  
  local p = vec3d(getMouseCursorPos())
  
  p = vec3d(transform.globalToLocal(o.lspace, p.x, p.y, p.z))
  
  d1 = Vector3D.dot(p - t2[1],t[1] - t2[1])
  d2 = Vector3D.dot(p - t2[2],t[2] - t2[2])
  d3 = Vector3D.dot(p - t2[3],t[3] - t2[3])
  
  local result = true
  
  if d1 < 0 then result = false end
  if d2 < 0 then result = false end
  if d3 < 0 then result = false end
  
  --clearTrace()
  --print(d1)
  --print(d2)
  --print(d3)
  
  t2[1] = t2[1] + vec3d(0,2,0)
  t2[2] = t2[2] + vec3d(0,2,0)
  t2[3] = t2[3] + vec3d(0,2,0)
  
  --print("" .. t2[1].x ..",".. t2[1].y ..",".. t2[1].z)
  --print("" .. t2[2].x ..",".. t2[2].y ..",".. t2[2].z)
  --print("" .. t2[3].x ..",".. t2[3].y ..",".. t2[3].z)
  
  glBeginTriangles()
    if result then
      colorGL(0,255,0,255)
    else
      colorGL(255,0,0,255)
    end
    vectorGL(t[1].x, t[1].y, t[1].z)
    vectorGL(t[2].x, t[2].y, t[2].z)
    vectorGL(t[3].x, t[3].y, t[3].z)
    
    colorGL(255,0,255,255)
    vectorGL(t2[1].x, t2[1].y, t2[1].z)
    vectorGL(t2[2].x, t2[2].y, t2[2].z)
    vectorGL(t2[3].x, t2[3].y, t2[3].z)
  glEnd()
end

-- editor remembers "preferred" column, tries to go to it
-- up and down use preferred column.

--[[
utIntersectionResult utIntersectionPosition(const mlVector3D & rayPoint, const mlVector3D & rayVector, const mlTriangle & plane)
{
  mlVector3D planeNormal = plane.Normal().Normalised();
  
  mlFloat rayLengthTowardPlane = rayVector * planeNormal;

  //rayLengthTowardPlane = mlFabs(rayLengthTowardPlane);
  
  if(rayLengthTowardPlane == 0.0f)
  {
    return utIntersectionResult();
  }
  
  mlVector3D planePointToRay = plane.a - rayPoint;
  
  mlFloat distanceToPlane = planePointToRay * planeNormal;
  
  //distanceToPlane = mlFabs(distanceToPlane);

  mlFloat lineLengthener = distanceToPlane / rayLengthTowardPlane;
  
  return utIntersectionResult(lineLengthener, rayPoint + (rayVector * lineLengthener));
}
]]

function rayModelIntersection(m,r)
  -- iterate all quads
  -- project to quad
  -- point in quad
end

do
  airplane.width = 50
  airplane.depth = 25

  airplane.render = function (o)
  
  renderInsideOutside(o)
  
  renderTriModel(model2)

  renderModel(planemodel)
  
  
  -- Grid
  glBeginLines()
    colorGL(0,255,0,255)
    for i=-o.width,o.width+1,5 do
      glVertex(i, 0.1, 0)
      glVertex(i, 0.1, o.depth)
    end
    for i=0,o.depth+1,5 do
      glVertex(-o.width, 0.1, i)
      glVertex( o.width, 0.1, i)
    end
  glEnd()
  
  -- pressable areas
  glBeginQuads()
    colorGL(255,0,255,255)
    vectorGL(0, 0, 0)
    colorGL(255,0,0,255)
    vectorGL(5, 0, 0)
    vectorGL(5, 0, 5)
    colorGL(0,0,255,255)
    vectorGL(0, 0, 5)
  glEnd()
  
  glBeginQuads()
    colorGL(0,255,0,255)
    vectorGL(5,  0, 0)
    vectorGL(10, 0, 0)
    vectorGL(10, 0, 5)
    vectorGL(5,  0, 5)
  glEnd()
  
  end
end







