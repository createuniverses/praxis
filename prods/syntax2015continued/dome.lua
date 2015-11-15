dome = dome or WidgetLib.newSimple()

dome.model = {}

do
 do dome.makeModel = function(r,step)
  local model = {}
  local pts1 = {}
  local pts2 = {}

  for a = 0,360,step do
    table.insert(pts1, vec3d(
        r * math.sin(deg2rad(a)),
        0,
        r * math.cos(deg2rad(a))))
    table.insert(pts2, vec3d(
        2 * r * math.sin(deg2rad(a)),
        0,
        2 * r * math.cos(deg2rad(a))))
  end

  for i=1,#pts1-1,1 do
    table.insert(model,
      { pts1[i],
        pts2[i],
        pts2[i+1],
        pts1[i+1]    } )
  end
  return model
 end end

 

 dome.model = dome.makeModel(12,5)
end

do
 function dome.renderModel(m)
  for i = 1,#m,1 do
    local q = m[i]
    glBeginQuads()
     colorGL(250,20,205,55)
     vectorGL(Vector3D.getArgs(q[1]))
     colorGL(100,50,200,155)
     vectorGL(Vector3D.getArgs(q[2]))
     colorGL(100,50,200,155)
     vectorGL(Vector3D.getArgs(q[3]))
     colorGL(40,30,220,155)
     vectorGL(Vector3D.getArgs(q[4]))
    glEnd()
  end

  if false then
    colorGL(0,255,0,255)
    local q = m[1]
    drawText3D("1", Vector3D.getArgs(q[1]))
    drawText3D("2", Vector3D.getArgs(q[2]))
    drawText3D("3", Vector3D.getArgs(q[3]))
    drawText3D("4", Vector3D.getArgs(q[4]))
  end
 end

 dome.t = 0
 
 dome.render = function (o)
  glPushMatrix()
  glTranslate(0,0,-50)
  o.renderModel(o.model)
  glPushMatrix()
   for i=1,10,1 do
    glTranslate(0,1*math.sin(deg2rad(dome.t)),0)
    glRotate(10 * math.sin(deg2rad(dome.t)),1,0,0)
    o.renderModel(o.model)
   end
  glPopMatrix()
  glPopMatrix()
  dome.t = dome.t + 5
 end

 dome.render = function (o)
  glPushMatrix()
  glTranslate(0,0,-50)
  o.renderModel(o.model)
  glPushMatrix()
   for i=1,10,1 do
    local a = 
    glTranslate(0,4,0)
    --glScale(math.sin(a),1,math.sin(a))
    glScale(0.99, 1, 0.9)
    --glRotate(10 * math.sin(deg2rad(dome.t)),1,0,0)
    o.renderModel(o.model)
   end
  glPopMatrix()
  glPopMatrix()
  dome.t = dome.t + 5
 end
end




