--edSetRenderMode(0)

--edSetTopMargin(0.25)
--edSetVisLines(5)

--edSetVisLines(40)
--edSetVisColumns(70)

glColor = colorGL

function glVec(v)
  vectorGL(v.x, v.y, v.z)
end

function pointOnCircle(angle, radius)
  local v = vec3d(radius * math.sin(angle),
                  radius * math.cos(angle),
                  0);
  return v
end

------------------

if spirowidget == nil then
  spirowidget = WidgetLib.newSimple()
end

--transform.setTranslation(spirowidget.lspace, getMouseCursorPos())
--makePositionSaver("spirowidget")
spirowidget.lspace = transform.new()
transform.setTranslation(spirowidget.lspace, 163,20,173)

--setBufferName("spirograph3d.lua")

transform.scale(spirowidget.lspace, 30,10,30)
transform.translate(spirowidget.lspace, 0,180,0)

maxstreamersegments = 300

streamer = Queue.new()
function addPointToStreamer(s, p)
  Queue.pushfirst(s, p)
  if Queue.size(s) > maxstreamersegments then
    Queue.poplast(s)
  end
end

function addPointToStreamer2(s, p, max)
  Queue.pushfirst(s, p)
  if Queue.size(s) > max then
    Queue.poplast(s)
  end
end

function renderStreamer(s)
  beginLinGL()
  for i=1,Queue.size(s)-1,1 do
    local p1 = Queue.get(s,i)
    local p2 = Queue.get(s,i+1)
    glVec(p1)
    glVec(p2)
  end
  endGL()
  
  if false then
  for i=1,Queue.size(s)-1,1 do
    local draw = true
    if i > 200 and i > math.random(300) then
      --draw = false
    end
    if draw then
      local p1 = Queue.get(s,i)
      local p2 = Queue.get(s,i+1)
      crazyLine(p1,p2,2,5)
    end
  end
  end
end

function renderStreamerCar(s)
  beginLinGL()
  if Queue.size(s) > 50 then
    local i = 40
    local p1 = Queue.get(s,i)
    local p2 = Queue.get(s,i+5)
    local p3 = p1 * 0.8
    local p4 = p2 * 0.8
    glVec(p1)
    glVec(p3)
    glVec(p2)
    glVec(p4)
    glVec(p3)
    glVec(p4)
  end
  endGL()
end

do
  spirowidget.cogs = {}
  local cogs = spirowidget.cogs
  cogs[1] = { radius = 10, angle = 0, speed = 1 }
  cogs[2] = { radius = 5, angle = 0, speed = 4 }
  cogs[3] = { radius = 7, angle = 0, speed = 2 }
  cogs[4] = { radius = 12, angle = 0, speed = 3 }

  cogs[1] = { radius = 10, angle = 0, speed = 1 }
  cogs[2] = { radius = 5, angle = 0, speed = 4 }
  cogs[3] = { radius = 7, angle = 0, speed = 7 }
  cogs[4] = { radius = 9, angle = 0, speed = 4 }
  --cogs[5] = { radius = 11, angle = 0, speed = 2.5 }
  --cogs[6] = { radius = 4.3, angle = 0, speed = 3.7 }
  --cogs[7] = { radius = 8, angle = 0, speed = 2.3 }


  for i=1,#cogs,1 do
    cogs[i].normal = cogs[i].normal or vec3d(0,1,0)
    cogs[i].zero = cogs[i].zero or vec3d(0,0,1)
  end
  
  cogs[1].zero = Vector3D.fromEulerAngles(
                        vec3d(0,0,1),
                        0,deg2rad(60))
  cogs[1].normal = Vector3D.fromEulerAngles(
                        vec3d(0,1,0),
                        0,deg2rad(60))

  cogs[2].zero = Vector3D.fromEulerAngles(
                        vec3d(0,0,1),
                        0,deg2rad(90))
  cogs[2].normal = Vector3D.fromEulerAngles(
                        vec3d(0,1,0),
                        0,deg2rad(90))

  cogs[3].zero = Vector3D.fromEulerAngles(
                        vec3d(0,0,1),
                        0,deg2rad(170))
  cogs[3].normal = Vector3D.fromEulerAngles(
                        vec3d(0,1,0),
                        0,deg2rad(170))

  streamer = Queue.new()
end

do
  spirowidget.cogs[1].speed = 1
  spirowidget.cogs[2].speed = 4
  spirowidget.cogs[3].speed = 7
  streamer = Queue.new()
end

clearError()
continue()

do
 streamer = Queue.new()
 
 spirowidget.pos = {}
 
 function spirowidget.renderDisc(cog,pos)
   local s = 10
   local p1 = pos + cog.zero * cog.radius
   glColor(30,30,255,255)
   for a = 0,360-s,s do
     local p2 = Vector3D.rotate(
           p1, pos, pos + cog.normal,
           deg2rad(s))
     glColor(30,30,255,255)
     drawLine(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z)
     --glColor(230,30,55,255)
     --drawText3D("1:"..a, p1.x, p1.y, p1.z)
     --drawText3D("2:"..a, p2.x, p2.y, p2.z)
     p1 = p2
   end
 end

 function randomVec(d)
   local v = vec3d(math.random(d) - d*0.5,
                   math.random(d) - d*0.5,
                   math.random(d) - d*0.5)
   v = v * 0.01
   return v
 end

 function crazyLine(p1, p2, n, d)
   for i=1,n,1 do
     local a = p1 + randomVec(d)
     local b = p2 + randomVec(d)
     drawLine(a.x, a.y, a.z, b.x, b.y, b.z)
   end
 end
 
 function spirowidget.render(w)
  local cogs = w.cogs
  local p = w.pos
  
  glColor(190,190,10)

  p[1] = vec3d(0,0,0)
  local arm = p[1] + (cogs[1].zero * cogs[1].radius)
  arm = Vector3D.rotate(
           arm, p[1], p[1] + cogs[1].normal,
           cogs[1].angle)
  p[2] = arm

  glColor(190,190,10)
  if showarms then
    crazyLine(p[1], p[2], 10, 10)
  end
  --drawLine(p[1].x,p[1].y,p[1].z, p[2].x,p[2].y,p[2].z)
  if showdiscs then
    spirowidget.renderDisc(cogs[1],p[1])
  end

  for i=2,#cogs,1 do
    local arm = p[i] + (cogs[i].zero * cogs[i].radius)
    arm = Vector3D.rotate(
              arm, p[i], p[i] + cogs[i].normal,
              cogs[i].angle)

    local p1 = p[i]
    local p2 = arm

    p[i+1] = p2
    
    glColor(190,190,10)
    if showarms then
      crazyLine(p1, p2, 10, 10)
    end
    --drawLine(p1.x,p1.y,p1.z, p2.x,p2.y,p2.z)
    if showdiscs then
      spirowidget.renderDisc(cogs[i],p[i])
    end

    if i==#cogs then
      addPointToStreamer(streamer, vec3d(p2.x, p2.y, p2.z))
    end
  end

  glColor(255,255,255,255)
  renderStreamer(streamer)
  --renderStreamerCar(streamer)
  
  for i=1,#cogs,1 do
    cogs[i].angle = cogs[i].angle + ((cogs[i].speed / 180) * math.pi * 0.3)
  end
 end
end









