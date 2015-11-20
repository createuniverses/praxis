showFPS()

stage = 1
--edSetVisLines(20)
--edSetVisColumns(40)



function update()
  WidgetLib.callAll("update")

  if stage == 4 then
    airplane.followcam = false
    airplane.pilot = airplane.allstoppilot
  elseif stage == 3 then
    airplane.followcam = true
    showdiscs = false
    showarms = false
  elseif stage == 2 then
    do
      airplane.followcam = false
      --lookAt(transform.getTranslation(airplane.lspace))
      showdiscs = true
      showarms = true
    end
  elseif stage == 1 then
    airplane.pilot = airplane.normalpilot
  else
    airplane.pilot = airplane.takeoffpilot
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
    os.exit()
  end
end

function render()
  WidgetLib.renderAll()

  --[[glColor(0, 0, 0)
  glBeginQuads()
    glVertex(-500,-2,-500)
    glVertex(-500,-2, 500)
    glVertex( 500,-2, 500)
    glVertex( 500,-2,-500)
  glEnd()]]
  
  -- each name appears as the plane moves forward
  if stage == 0 then
--    renderGreets2()
  end

  glColor(150 + math.random(100),110,20)

  renderStreamer(airplane.lwing)
  renderStreamer(airplane.rwing)

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
end

--setBufferName("linux.lua")

windowedMode()

-- turn off the arch
archwidget.render_old = archwidget.render
archwidget.render = function (o) end


setPickSphere(true)
