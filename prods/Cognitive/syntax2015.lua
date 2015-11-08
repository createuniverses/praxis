
Widgets[2].width = 100

do
controls =
{
  pitchup = false,
  pitchdown = false,
  bankleft = false,
  bankright = false,
  thrust = false
}
end

stdkeyids["w"] = 87
stdkeyids["a"] = 65
--stdkeyids["s"] = 83
stdkeyids["d"] = 68
stdkeyids["e"] = 69

do
setKeyHandlerProgram(keymap2, stdkeyids["w"], 0, "controls.pitchup = true")
setKeyHandlerProgram(keymap2, stdkeyids["a"], 0, "controls.bankleft = true")
setKeyHandlerProgram(keymap2, stdkeyids["s"], 0, "controls.pitchdown = true")
setKeyHandlerProgram(keymap2, stdkeyids["d"], 0, "controls.bankright = true")
setKeyHandlerProgram(keymap2, stdkeyids["e"], 0, "controls.thrust = true")
setKeyHandlerProgram(keymap2, stdkeyids["c"], 0, "controls.thrust = false")

setKeyHandlerProgram(keymap2up, stdkeyids["w"], 0, "controls.pitchup = false")
setKeyHandlerProgram(keymap2up, stdkeyids["a"], 0, "controls.bankleft = false")
setKeyHandlerProgram(keymap2up, stdkeyids["s"], 0, "controls.pitchdown = false")
setKeyHandlerProgram(keymap2up, stdkeyids["d"], 0, "controls.bankright = false")
setKeyHandlerProgram(keymap2up, stdkeyids["e"], 0, "")
end

do
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
    if controls.thrust then
      transform.translate(o.lspace, forward.x, forward.y, forward.z)
    end

    side = vec3d(transform.side(o.lspace))
    forward = vec3d(transform.forward(o.lspace))
    up = vec3d(transform.up(o.lspace))

    transform.copy(transform.cameraBase(), o.lspace)
    transform.copy(transform.camera(), o.lspace)

    local offset = up * 5 + side * 5

    transform.translate(transform.camera(), offset.x, offset.y, offset.z)
  end
end

transform.copy(Widgets[2].lspace, transform.identity())
transform.copy(transform.cameraBase(), transform.identity())
transform.copy(transform.camera(), transform.identity())

--continue()
--clearError()
--clearTrace()
