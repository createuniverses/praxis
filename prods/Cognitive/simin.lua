setFloorGrid(false)

do
  setCamPos(-340,
            165,
            250)
  lookAt(0,0,250)
end

angle = 0
colort = 0
function render()
  colorGL(100 + math.sin(colort*1) * 50 + 50,
          100 + math.sin(colort*2) * 50 + 50,
          100 + math.sin(colort*3) * 50 + 50,
          255)
  colort = colort + math.pi * 0.025
  --colorGL(math.random(255),
  --        math.random(255),
  --        math.random(255),
  --        255)
  angle = angle + 2
  for i=0,2,1 do
  glPushMatrix()
  glTranslate(0,0,250)
  glRotate(angle, 1,1,1)
  glRotate(angle, 0,1,1)
  glTranslate(0,0,-250)
  --glScale(i,1,i)
  glScale(10,1,10)
  drawText3DStroked("Happy Birthday\n   Simin!!", 0,i * 40,0)
  --drawText3DStroked("Happy Birthday\n   Simin!!", 0,0,0)
  glPopMatrix()
  end
end
