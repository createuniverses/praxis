
Widgets[2].width = 100

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

stdkeyids["w"] = 87
stdkeyids["a"] = 65
stdkeyids["s"] = 83
stdkeyids["d"] = 68
stdkeyids["e"] = 69
stdkeyids["q"] = 81

stdkeyids["r"] = 82
stdkeyids["f"] = 70

--print(string.byte('F'))


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
  Widgets[2].speed = 0
  
  Widgets[2].update = function (o)
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

    if true then
      --transform.copy(transform.cameraBase(), o.lspace)
      transform.copy(transform.camera(), o.lspace)

      local offset = up * 5 + side * 0
  
      transform.translate(transform.camera(), offset.x, offset.y, offset.z)
    end
    
    o.speed = o.speed + (controls.thrust - o.speed) * 0.1
    
    transform.normalise(o.lspace)
    transform.normalise(transform.camera())
  end
end

transform.copy(Widgets[2].lspace, transform.identity())
transform.copy(transform.cameraBase(), transform.identity())
transform.copy(transform.camera(), transform.identity())

continue()
clearError()
clearTrace()

--print2(getFunction(Widgets[2].render))

do
  Widgets[2].render = function (o)
  
  -- Grid
  glBeginLines()
    colorGL(0,255,0,255)
    for i=-o.width,o.width+1,5 do
      glVertex(i, 0.1, 0)
      glVertex(i, 0.1, o.depth)
    end
    for i=0,o.depth+1,5 do
      glVertex(-o.width, 0.1, i)
      glVertex( o.width, 0.1, i)
    end
  glEnd()
  
  -- pressable areas
  glBeginQuads()
    colorGL(255,0,255,255)
    vectorGL(0, 0, 0)
    vectorGL(5, 0, 0)
    vectorGL(5, 0, 5)
    vectorGL(0, 0, 5)
  glEnd()
  
  glBeginQuads()
    colorGL(0,255,0,255)
    vectorGL(5,  0, 0)
    vectorGL(10, 0, 0)
    vectorGL(10, 0, 5)
    vectorGL(5,  0, 5)
  glEnd()
  
  glBuildStencil(0)
  
  o:renderStencil()
  
  glDrawWithinStencil()
  
  glBeginQuads()
    colorGL(255,0,0,255)
    vectorGL(0,       -20, 0)
    vectorGL(o.width, -20, 0)
    vectorGL(o.width, -20, o.depth)
    vectorGL(0,       -20, o.depth)
  glEnd()
  
  glRemoveStencil()
  end
end
