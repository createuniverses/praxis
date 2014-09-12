lookDown()
  
function cpos(c,r,a)
  return c + (vec3d(math.sin(a),math.cos(a), 0) * r)
end

function deg2rad(r)
  return r * (math.pi/180)
end

texture.beginDrawing(meter)

glMatrixModeProjection()
glPushMatrix()
glLoadIdentity()
glOrtho(0,100,0,100,0,10)

glMatrixModeModelView()
glPushMatrix()
glLoadIdentity();

glClear()

-- test: declare these local

center = vec3d(50,50,-3)

angstep = deg2rad(5)
angrange = deg2rad(130)

meterradius = 40
meterwidth = 10

clipoffset = vec3d(0,-20,0)
clipcenter = center + clipoffset
clipcenter.z = -2
clipradius1 = meterradius - clipoffset.y - meterwidth
clipradius2 = clipradius1 + meterwidth
angstepclip = deg2rad(2)

-- draw the meter ticks

colorGL(255,255,255,255)
for i=-angrange*0.5,angrange*0.5+0.0001,angstep do
  local pos1 = cpos(center,100, i)
  local pos2 = cpos(center,5,   i)
  drawLine(pos1.x,pos1.y,center.z,pos2.x, pos2.y,center.z)
end

-- draw the stencilling geometry

beginTriGL()
--colorGL(255,0,0,255)
--colorGL(0,0,0,255)
--colorGL(0,0,100,100)
--for i=0,2*math.pi,angstepclip do
for i=-angrange*0.5,angrange*0.5+0.0001,angstepclip do
  local pos1 = cpos(clipcenter,clipradius1,i)
  local pos2 = cpos(clipcenter,clipradius1,i+angstepclip)
  colorGL(0,0,100,100)
  vectorGL(clipcenter.x,clipcenter.y,clipcenter.z)
  vectorGL(pos1.x,pos1.y,pos1.z)
  vectorGL(pos2.x,pos2.y,pos2.z)
  
  local pos3 = cpos(clipcenter,clipradius2,i)
  local pos4 = cpos(clipcenter,clipradius2,i+angstepclip)
  local pos5 = cpos(clipcenter,clipradius2 + 50,i)
  local pos6 = cpos(clipcenter,clipradius2 + 50,i+angstepclip)
  
  colorGL(0,100,0,100)
  vectorGL(pos5.x,pos5.y,pos5.z)
  vectorGL(pos3.x,pos3.y,pos3.z)
  vectorGL(pos4.x,pos4.y,pos4.z)
  
  vectorGL(pos5.x,pos5.y,pos5.z)
  vectorGL(pos4.x,pos4.y,pos4.z)
  vectorGL(pos6.x,pos6.y,pos6.z)
end

for i=angrange*0.5,angrange*0.5+(math.pi * 2) - angrange - 0.0001,angstepclip do
  local pos1 = cpos(center,100,i)
  local pos2 = cpos(center,100,i+angstepclip)
  --local pos2 = cpos(center,5  ,i+angstepclip)
  colorGL(100,0,0,100)
  vectorGL(center.x,center.y,center.z)
  --vectorGL(clipcenter.x,clipcenter.y,clipcenter.z)
  vectorGL(pos1.x,pos1.y,pos1.z)
  vectorGL(pos2.x,pos2.y,pos2.z)
end

endGL()

colorGL(255,255,255,255)

drawLine(center.x - 1, center.y, -1,
         center.x + 1, center.y, -1)

drawLine(center.x, center.y - 1, -1,
         center.x, center.y + 1, -1)

drawText3D("Amps",45,65,-1)
-- need a drawText2DCentered()

glMatrixModeModelView()
glPopMatrix()
glMatrixModeProjection()
glPopMatrix()

texture.endDrawing(meter)
