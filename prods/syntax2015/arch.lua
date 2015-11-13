
archwidget = archwidget or WidgetLib.newSimple()

do
  archmodel = {}

  local points = {}
  for a = -90,90,10 do
    local p = vec3d(math.sin(deg2rad(a)), math.cos(deg2rad(a)), 0)
    p = p * 50
    table.insert(points, p)
  end
  
  local displacement = vec3d(0,0,5)
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
    table.insert(archmodel, {p1, p2, p3, p4})
  end
  
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
      table.insert(archmodel, {p1, p2, p3, p4})
    end
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
  
  function archwidget.renderModel(m)
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
  archwidget.render = function (o)
    archwidget.renderModel(archmodel)
  end
end








