
Widgets[2].width = 100

do
controls =
{
  pitchup = false,
  pitchdown = false,
  bankleft = false,
  bankright = false
}
end

stdkeyids["w"] = 87
stdkeyids["a"] = 65
--stdkeyids["s"] = 83
stdkeyids["d"] = 68

do
setKeyHandlerProgram(keymap2, stdkeyids["w"], 0, "controls.pitchup = true")
setKeyHandlerProgram(keymap2, stdkeyids["a"], 0, "controls.bankleft = true")
setKeyHandlerProgram(keymap2, stdkeyids["s"], 0, "controls.pitchdown = true")
setKeyHandlerProgram(keymap2, stdkeyids["d"], 0, "controls.bankright = true")

setKeyHandlerProgram(keymap2up, stdkeyids["w"], 0, "controls.pitchup = false")
setKeyHandlerProgram(keymap2up, stdkeyids["a"], 0, "controls.bankleft = false")
setKeyHandlerProgram(keymap2up, stdkeyids["s"], 0, "controls.pitchdown = false")
setKeyHandlerProgram(keymap2up, stdkeyids["d"], 0, "controls.bankright = false")
end

do
  Widgets[2].update = function (o)
    local side = vec3d(transform.side(o.lspace))
    local forward = vec3d(transform.forward(o.lspace))
    if controls.pitchup then
      transform.rotate(o.lspace, 1 * (math.pi/180),side.x,side.y,side.z)
    end
    if controls.pitchdown then
      transform.rotate(o.lspace, -1 * (math.pi/180),side.x,side.y,side.z)
    end
    if controls.bankleft then
      transform.rotate(o.lspace, 1 * (math.pi/180),forward.x, forward.y, forward.z)
    end
    if controls.bankright then
      transform.rotate(o.lspace, -1 * (math.pi/180),forward.x, forward.y, forward.z)
    end
  end
end
