
airplane = airplane or WidgetLib.newSimple()

transform.setTranslation(airplane.lspace, 150,250,150)

do
controls =
{
  pitchup = false,
  pitchdown = false,
  bankleft = false,
  bankright = false,
  yawleft = false,
  yawright = false,
  pitch = 0,
  bank = 0,
  yaw = 0,
  thrust = 0
}
end

if platform() == "windows" then
  stdkeyids["w"] = 87
  stdkeyids["a"] = 65
  stdkeyids["s"] = 83
  stdkeyids["d"] = 68
  stdkeyids["e"] = 69
  stdkeyids["q"] = 81
  
  stdkeyids["r"] = 82
  stdkeyids["f"] = 70
end

if platform() == "linux" then
  stdkeyids["a"] = 38
  stdkeyids["s"] = 39
  stdkeyids["d"] = 40
  stdkeyids["f"] = 41
  stdkeyids["q"] = 24
  stdkeyids["w"] = 25
  stdkeyids["e"] = 26
  stdkeyids["r"] = 27
end


do
setKeyHandlerProgram(keymap2, stdkeyids["w"], 0, "controls.pitchup = true")
setKeyHandlerProgram(keymap2, stdkeyids["a"], 0, "controls.bankleft = true")
setKeyHandlerProgram(keymap2, stdkeyids["s"], 0, "controls.pitchdown = true")
setKeyHandlerProgram(keymap2, stdkeyids["d"], 0, "controls.bankright = true")
setKeyHandlerProgram(keymap2, stdkeyids["q"], 0, "controls.yawleft = true")
setKeyHandlerProgram(keymap2, stdkeyids["e"], 0, "controls.yawright = true")

setKeyHandlerProgram(keymap2, stdkeyids["r"], 0, "controls.thrust = controls.thrust + 0.5")
setKeyHandlerProgram(keymap2, stdkeyids["f"], 0, "controls.thrust = controls.thrust - 0.5")

setKeyHandlerProgram(keymap2up, stdkeyids["w"], 0, "controls.pitchup = false")
setKeyHandlerProgram(keymap2up, stdkeyids["a"], 0, "controls.bankleft = false")
setKeyHandlerProgram(keymap2up, stdkeyids["s"], 0, "controls.pitchdown = false")
setKeyHandlerProgram(keymap2up, stdkeyids["d"], 0, "controls.bankright = false")
setKeyHandlerProgram(keymap2up, stdkeyids["q"], 0, "controls.yawleft = false")
setKeyHandlerProgram(keymap2up, stdkeyids["e"], 0, "controls.yawright = false")
end

do
  airplane.speed = 0
  airplane.followcam = true
end

airplane.target = vec3d(0,0,0)

do airplane.normalpilot = function (o)
 local p = Queue.get(streamer, 1)
 if p~=nil then
  p = cvec3d(p)
  p.x, p.y, p.z = transform.localToGlobal(spirowidget.lspace, p.x, p.y, p.z)
  o.target = cvec3d(p)
  p.x, p.y, p.z = transform.globalToLocal(o.lspace, p.x, p.y, p.z)
  p = p + vec3d(0,5,0)
  
  local dist = Vector3D.magnitude(p)
  controls.adjust = false
  controls.bankleft = false
  controls.bankright = false
  if p.x > 5 then
   controls.bankleft = true
   controls.adjust = true
  end
  if p.x < -5 then
   controls.bankright = true
   controls.adjust = true
  end
  
  controls.pitchup = false
  controls.pitchdown = false
  if p.y > 5 then
   controls.pitchdown = true
   controls.adjust = true
  end
  if p.y < -5 then
   controls.pitchup = true
   controls.adjust = true
  end
  
  if p.z < 0 then
   controls.pitchup = true
   controls.adjust = true
  end
  
  if controls.adjust then
    controls.thrust = 3.55
  else
    controls.thrust = 7.55
  end

 end
end end

takeoffthrust = 0.6

do airplane.takeoffpilot = function (o)
  controls.pitchup = false
  controls.pitchdown = false
  controls.bankleft = false
  controls.bankright = false
  controls.yawleft = false
  controls.yawright = false
  controls.pitch = 0
  controls.bank = 0
  controls.yaw = 0
  controls.thrust = takeoffthrust
end end

airplane.pilot = airplane.normalpilot

do
  function dampTo(s,t,d)
    return s + ((t - s) * d)
  end

  airplane.update = function (o)
    airplane.pilot(o)
    
    local side = vec3d(transform.side(o.lspace))
    local forward = vec3d(transform.forward(o.lspace))
    local up = vec3d(transform.up(o.lspace))

    local rs = 5

    if controls.pitchup then
      controls.pitch = dampTo(controls.pitch, rs, 0.1)
    elseif controls.pitchdown then
      controls.pitch = dampTo(controls.pitch, -rs, 0.1)
    else
      controls.pitch = dampTo(controls.pitch, 0, 0.1)
    end
    if controls.bankleft then
      controls.bank = dampTo(controls.bank, rs*0.5, 0.1)
    elseif controls.bankright then
      controls.bank = dampTo(controls.bank, -rs*0.5, 0.1)
    else
      controls.bank = dampTo(controls.bank, 0, 0.1)
    end
    if controls.yawleft then
      controls.yaw = dampTo(controls.yaw, -rs, 0.1)
    elseif controls.yawright then
      controls.yaw = dampTo(controls.yaw, rs, 0.1)
    else
      controls.yaw = dampTo(controls.yaw, 0, 0.1)
    end

    transform.rotate(o.lspace, controls.pitch * (math.pi/180),side.x,side.y,side.z)
    transform.rotate(o.lspace, controls.bank * (math.pi/180),forward.x, forward.y, forward.z)
    transform.rotate(o.lspace, controls.yaw * (math.pi/180),up.x, up.y, up.z)
    
    local vel = forward * o.speed
    transform.translate(o.lspace, vel.x, vel.y, vel.z)

    side = vec3d(transform.side(o.lspace))
    forward = vec3d(transform.forward(o.lspace))
    up = vec3d(transform.up(o.lspace))

    o.camerayaw = o.camerayaw or 0
    o.camerayawctrl = o.camerayawctrl or 0
    o.camerapitch = o.camerapitch or 0
    o.camerapitchctrl = o.camerapitchctrl or 0

    if airplane.followcam then
      transform.copy(transform.camera(), o.lspace)

      local offset = up * 5 + side * 0
  
      transform.translate(transform.camera(), offset.x, offset.y, offset.z)
      --lookAt(Vector3D.getArgs(o.target))

      transform.rotate(transform.camera(),
        o.camerapitch,side.x, side.y, side.z)

      transform.rotate(transform.camera(),
        o.camerayaw,up.x, up.y, up.z)
      
      local camspace =
        vec3d(transform.globalToLocal(
                transform.camera(),
                Vector3D.getArgs(o.target)))
      
      if camspace.x < -10 then
        o.camerayawctrl = dampTo(
            o.camerayawctrl,
            deg2rad(-2), 0.1)
      elseif camspace.x > 10 then
        o.camerayawctrl = dampTo(
            o.camerayawctrl,
            deg2rad(2), 0.1)
      else
        o.camerayawctrl = dampTo(
            o.camerayawctrl,
            0, 0.1)
      end

      if camspace.y < -10 then
        o.camerapitchctrl = dampTo(
            o.camerapitchctrl,
            deg2rad(-2), 0.1)
      elseif camspace.y > 10 then
        o.camerapitchctrl = dampTo(
            o.camerapitchctrl,
            deg2rad(2), 0.1)
      else
        o.camerapitchctrl = dampTo(
            o.camerapitchctrl,
            0, 0.1)
      end

      o.camerayaw = o.camerayaw + o.camerayawctrl
      if o.camerayaw < deg2rad(-30) then
         o.camerayaw = deg2rad(-30) end
      if o.camerayaw > deg2rad(30) then
         o.camerayaw = deg2rad(30) end

      o.camerapitch = o.camerapitch + o.camerapitchctrl
      if o.camerapitch < deg2rad(-5) then
         o.camerapitch = deg2rad(-5) end
      if o.camerapitch > deg2rad(5) then
         o.camerapitch = deg2rad(5) end
    end
    
    o.speed = o.speed + (controls.thrust - o.speed) * 0.1
    
    transform.normalise(o.lspace)
    transform.normalise(transform.camera())
  end
  
  planemodel = {}
  function deg2rad(a)
    return math.pi * (a/180)
  end
  function rad2deg(a)
    return 180 * (a/math.pi)
  end
  local flip = function(p)
    local p2 = {}
    for i=1,4,1 do
      table.insert(
         p2,
         vec3d(p[i].x*-1, p[i].y, p[i].z))
    end
    return p2
  end
  table.insert(planemodel,
    { vec3d( 1, 0,  1),
      vec3d( 0, 0, 10),
      vec3d(15, -3,  5),
      vec3d(15, -3,  0) })
  table.insert(planemodel, flip(planemodel[1]))
  
  function renderModel(m)
    glBeginQuads()
    for i=1,#m,1 do
      colorGL(250,20,205,255)
      vectorGL(m[i][1].x, m[i][1].y, m[i][1].z)
      colorGL(100,50,200,255)
      vectorGL(m[i][2].x, m[i][2].y, m[i][2].z)
      colorGL(100,50,200,255)
      vectorGL(m[i][3].x, m[i][3].y, m[i][3].z)
      colorGL(40,30,220,255)
      vectorGL(m[i][4].x, m[i][4].y, m[i][4].z)
    end
    glEnd()
  end

  airplane.lwing = Queue.new()
  airplane.rwing = Queue.new()
  airplane.smax = 50
  
  airplane.render = function (o)
    renderModel(planemodel)
    addPointToStreamer2(o.lwing,
      vec3d(transform.localToGlobal(o.lspace, 15,-3,0)),
      o.smax)
    addPointToStreamer2(o.rwing,
      vec3d(transform.localToGlobal(o.lspace, -15,-3,0)),
      o.smax)
   if true then
    local p = Queue.get(streamer, 1)
    if p~=nil then
      p = cvec3d(p)
      p.x, p.y, p.z = transform.localToGlobal(spirowidget.lspace, p.x, p.y, p.z)
      p.x, p.y, p.z = transform.globalToLocal(o.lspace, p.x, p.y, p.z)
      p = p + vec3d(0,5,0)
      glColor(255,50,50,255)
      glBeginTriangles()
        glVertex(p.x - 10, p.y, p.z)
        glVertex(p.x + 10, p.y, p.z)
        glVertex(p.x, p.y+10, p.z+0)
      glEnd()
      --drawLine(0,0,0,p.x,p.y,p.z)
      --drawText3D("here", p.x,p.y,p.z)
    end
   end
  end
end













