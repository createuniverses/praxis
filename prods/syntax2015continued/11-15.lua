print2(getFunction(update))

stages = {}
do
 stages[4] = function ()
    airplane.followcam = false
    airplane.pilot = airplane.allstoppilot
 end
 stages[3] = function ()
    airplane.followcam = true
    showdiscs = false
    showarms = false
 end

 stages[2] = function ()
    do
      airplane.followcam = false
      lookAt(transform.getTranslation(airplane.lspace))
      showdiscs = true
      showarms = true
    end
 end

 stages[1] = function ()
    airplane.pilot = airplane.normalpilot
 end
end

function update()
  WidgetLib.callAll("update")
  
  if getMp3Time() > 117 then
    stages[4]()
  elseif getMp3Time() > 105.6 then
    stages[3]()
  elseif getMp3Time() > 79 then
    stages[2]()
  elseif getMp3Time() > 16 then
    stages[1]()
  end
  --clearTrace()
  --print(""..getMp3Time())
  
  for i=1,#skythings,1 do
    local thing = skythings[i]
    local planepos = vec3d(transform.getTranslation(airplane.lspace))
    local tween = thing.p - planepos
    local dist = Vector3D.magnitude(tween)
    tween = Vector3D.normalize(tween)
    if dist < (30 + thing.r) then
      thing.p = planepos + (tween * (30+thing.r))
    end
  end
  
  if isMp3Playing() == false then
--    os.exit()
  end
end

do
 playMp3()
 airplane.followcam = true
 airplane.pilot = airplane.takeoffpilot
end

clearError()
clearTrace()

hideTrace()

print2(getFunction(WidgetLib.renderAll))
function WidgetLib.renderAll()
  for k,v in pairs(Widgets) do
    if v.lspace ~= nil then
      glPushMatrix()
      glApplyTransform(v.lspace)
      v["render"](v)
      -- render bounding box as an option
      glPopMatrix()
    else
      v["render"](v)
    end
    if v["renderGlobal"] ~= nil then
      v["renderGlobal"](v)
    end
  end
end

function airplane.renderGlobal(o)
  renderStreamer(o.lwing)
  renderStreamer(o.rwing)
end


setBufferName("11-15.lua")

continue()

function makeCodeLogBuffer()
  parentBufferName = getBufferName()
  local sText = ""
  for i=1,#codelog,1 do
    sText = sText .. "Entry " .. i .. "\n"
    sText = sText .. codelog[i] .. "\n\n"
  end
  newBuffer("codelog.txt")
  setBufferText(sText)
  edSetPosition(#sText - 1)
end

showTrace()
clearTrace()

makeCodeLogBuffer()
continue()


stopMp3()

windowedMode()
airplane.followcam = true

-- move airplane so its last
for i=1,#Widgets,1 do
  if Widgets[i] == airplane then
    print2(i)
  end
end
1
table.remove(Widgets, 1)
table.insert(Widgets, airplane)

print2(getFunction(render))
function render()
  --[[glColor(0, 0, 0)
  glBeginQuads()
    glVertex(-500,-2,-500)
    glVertex(-500,-2, 500)
    glVertex( 500,-2, 500)
    glVertex( 500,-2,-500)
  glEnd()]]
  
  -- each name appears as the plane moves forward
  if getMp3Time() < 16 then
    renderGreets2()
  end

  glColor(150 + math.random(100),110,20)

  -- moved to airplane.renderGlobal
  --renderStreamer(airplane.lwing)
  --renderStreamer(airplane.rwing)

  for i=1,#skythings,1 do
    local thing = skythings[i]
    glPushMatrix()
      glTranslate(Vector3D.getArgs(thing.p))
      glColor(200,200,0)
      glutSolidSphere(thing.r)
      glColor(100,100,100)
      glutWireSphere(thing.r + 1)
    glPopMatrix()
  end
  
  WidgetLib.renderAll()
end



