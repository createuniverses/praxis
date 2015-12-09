dome = WidgetLib2.newSimple("dome")
--dome = dome or {}

dome.model = {}

do
 do dome.makeModel = function(r,step)
  local model = {}
  local pts1 = {}
  local pts2 = {}

  for a = 0,180,step do
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
 dome.color1 = 
 {
   r = 110,
   g = 0,
   b = 200,
   a = 255
 }

 dome.color2 =
 {
   r = dome.color1.r,
   g = dome.color1.g,
   b = dome.color1.b,
   a = dome.color1.a
 }

 dome.t2 = 20

 dome.color1.r,
 dome.color1.g,
 dome.color1.b = angleToColor(dome.t2,150,1)
 function glColor2(c)
  colorGL(c.r, c.g, c.b, c.a)
 end
 
 function mkCol(r,g,b)
   local col = {}
   col.r = r
   col.g = g
   col.b = b
   col.a = 255
   return col
 end
 
 do
   local r = 50
   dome.color1 = mkCol(angleToColor( 0,r,1))
   dome.color2 = mkCol(angleToColor(10,r,1))
   dome.color3 = mkCol(angleToColor(20,r,1))
   dome.color4 = mkCol(angleToColor(30,r,1))
 end

 function dome.update(o)
   dome.t2 = (dome.t2 + 5) % 360
   local t2 = dome.t2
   local t3 = (dome.t2 + 120) % 360
   local t4 = (dome.t2 + 120) % 360
   local t5 = (dome.t2 + 160) % 360
   local r = 50
   dome.color1 = mkCol(angleToColor(t5,r,1))
   dome.color2 = mkCol(angleToColor(t4,r,1))
   dome.color3 = mkCol(angleToColor(t4,r,1))
   dome.color4 = mkCol(angleToColor(t5,r,1))
 end
 clearError()
 continue()
end


do
 function dome.renderModel(m)
  for i = 1,#m,1 do
    local q = m[i]
    glBeginQuads()
     if i%2 == 0 then
     glColor2(dome.color1)
     vectorGL(Vector3D.getArgs(q[1]))
     glColor2(dome.color2)
     vectorGL(Vector3D.getArgs(q[2]))
     glColor2(dome.color3)
     vectorGL(Vector3D.getArgs(q[3]))
     glColor2(dome.color4)
     vectorGL(Vector3D.getArgs(q[4]))
     else
     glColor2(dome.color4)
     vectorGL(Vector3D.getArgs(q[1]))
     glColor2(dome.color3)
     vectorGL(Vector3D.getArgs(q[2]))
     glColor2(dome.color2)
     vectorGL(Vector3D.getArgs(q[3]))
     glColor2(dome.color1)
     vectorGL(Vector3D.getArgs(q[4]))
     end
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
  --glRotate(10 * math.sin(deg2rad(dome.t * 2)),1,0,0)
  --o.renderModel(o.model)
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
  glRotate(dome.t *0.5,1,0,0)
  o.renderModel(o.model)
  glRotate(dome.t*1.5, 0,1,0)
  dome.renderseq(dome)
  glPopMatrix()
  dome.t = dome.t + 5
 end

 dome.renderseq2 = function (o)
  glPushMatrix()
    
    glRotate(90,1,0,0)
    glTranslate(0,20,0)
   for i=1,10,1 do
    glTranslate(0,4,0)
    glRotate(dome.t*0.3, 0,1,0)
    --glScale(math.sin(a),1,math.sin(a))
    glScale(0.9, 1, 0.8)
    glRotate(20 * math.sin(deg2rad(dome.t)),1,0,0)
    o.renderModel(o.model)
   end
  glPopMatrix()
 end

 dome.renderseq = function (o)
  glPushMatrix()
   for i=1,10,1 do
    glTranslate(0,4,0)
    glRotate(dome.t*0.3, 0,1,0)
    --glScale(math.sin(a),1,math.sin(a))
    glScale(0.9, 1, 0.8)
    glRotate(20 * math.sin(deg2rad(dome.t)),1,0,0)
    if i == 2 or i == 4 then
      o.renderseq2(dome)
    else
      o.renderModel(o.model)
    end
   end
  glPopMatrix()
 end
end


