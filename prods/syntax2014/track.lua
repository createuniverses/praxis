
track = {}

function trak.init()
  for i=1,50,1 do
    track[i] = transform.new()
    transform.applyTranslation(track[i], 0,0,10)
  end

  ang = 0.01
  ang2 = 0.01

  framenum = 1
end

function trak.render()
  --local t = transform.new()
  local s = 10
  glPushMatrix()
  for i=1,#track,1 do
    glApplyTransform(track[i])
    
    movePen(s*-0.5,1,s)
    drawTo(s*-0.5,1,0)
    drawTo(s*0.5,1,0)
    drawTo(s*0.5,1,s)
    drawTo(s*-0.5,1,s)
  end
  glPopMatrix()
end

function trak.update()
  for k = 1,5,1 do
  local i = math.random(50)
  if math.random(100) <= 100 then
    transform.rotate(track[i],math.pi * ang, math.pi * ang2)
  else
    transform.rotate(track[i],math.pi * -ang, 0)
  end
  end
  if framenum % 3 == 0 then
  local i = math.random(50)
    transform.applyTranslation(track[i], 0,0,5)
  end
  if framenum % 100 == 0 then
    ang = ang * -1
    ang2 = ang2 * -1
  end
  if framenum % 1000 == 0 then
    for i=1,50,1 do
    track[i] = transform.new()
    transform.applyTranslation(track[i], 0,0,10)
    end
  end

  framenum = framenum + 1
  local camRadius = 200
  setCamPos(camRadius * math.sin(framenum * math.pi * 0.03),
            50,
            camRadius * math.cos(framenum * math.pi * 0.03))
  lookAt(0,-100,0)
end
