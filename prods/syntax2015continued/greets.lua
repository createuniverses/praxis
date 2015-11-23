
names = {{n = "Greetings to"},
         {n = "cTrix"},
         {n = "jimage"},
         {n = "Mudlord"},
         {n = "Reload"},
         {n = "Voltage"},
         {n = "TrinaryLogic"},
         {n = "Duck"},
         {n = "Chicken"},
         {n = "jazzcat"},
         {n = "conjuror"},
         {n = "Ript"}}
--         {n = "Aday"},
--         {n = "Brannigan"},
--         {n = "gaia"}}
         
for i=1,#names,1 do
  local name = names[i]
  name.tpos = vec3d(0,0,i*20)
  name.pos = cvec3d(name.tpos)
  name.pos.y = -10
  name.triggered = false
end


function renderGreets2()
  glPushMatrix()
  glTranslate(-30,20,100)
  --glRotate(-90,1,0,0)
  glColor(255,255,255)
  
  for i=1,#names,1 do
    local n = names[i]
    if n.triggered then
      glPushMatrix()
      glTranslate(Vector3D.getArgs(n.pos))
      glRotate(-90,1,0,0)
      drawText3DStroked(n.n, 0,0,0)
      glPopMatrix()
      n.pos.y = n.pos.y + 0.5
    else
      local tdist = 20
      local diff = getPlanePos() - vec3d(n.tpos.x, 0, n.tpos.z)
      local d = Vector3D.magnitude(diff)
      if d < tdist then n.triggered = true end
    end
  end
  
  glPopMatrix()
end
