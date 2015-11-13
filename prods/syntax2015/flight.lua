
airplane = airplane or WidgetLib.newSimple()

do
controls =
{
  pitchup = false,
  pitchdown = false,
  bankleft = false,
  bankright = false,
  yawleft = false,
  yawright = false,
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
  airplane.followcam = false
  
  airplane.update = function (o)
    local side = vec3d(transform.side(o.lspace))
    local forward = vec3d(transform.forward(o.lspace))
    local up = vec3d(transform.up(o.lspace))

    if controls.pitchup then
      transform.rotate(o.lspace, 5 * (math.pi/180),side.x,side.y,side.z)
    end
    if controls.pitchdown then
      transform.rotate(o.lspace, -5 * (math.pi/180),side.x,side.y,side.z)
    end
    if controls.bankleft then
      transform.rotate(o.lspace, 5 * (math.pi/180),forward.x, forward.y, forward.z)
    end
    if controls.bankright then
      transform.rotate(o.lspace, -5 * (math.pi/180),forward.x, forward.y, forward.z)
    end
    if controls.yawleft then
      transform.rotate(o.lspace, -5 * (math.pi/180),up.x, up.y, up.z)
    end
    if controls.yawright then
      transform.rotate(o.lspace, 5 * (math.pi/180),up.x, up.y, up.z)
    end
    --if controls.thrust then
    --  transform.translate(o.lspace, forward.x, forward.y, forward.z)
    --end
    local vel = forward * o.speed
    transform.translate(o.lspace, vel.x, vel.y, vel.z)

    side = vec3d(transform.side(o.lspace))
    forward = vec3d(transform.forward(o.lspace))
    up = vec3d(transform.up(o.lspace))

    if airplane.followcam then
      --transform.copy(transform.cameraBase(), o.lspace)
      transform.copy(transform.camera(), o.lspace)

      local offset = up * 5 + side * 0
  
      transform.translate(transform.camera(), offset.x, offset.y, offset.z)
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
  
  airplane.render = function (o)
    renderModel(planemodel)
  end
end







