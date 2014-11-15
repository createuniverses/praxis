tracktransform = transform.new()

function addSegment()
  segment = WidgetLib.addRender(
  function (o)
    movePen(o.s*-0.5,1,0)
    drawTo(o.s*0.5,1,0)
    drawTo(o.s*0.5,1,o.s)
    drawTo(o.s*-0.5,1,o.s)
    drawTo(o.s*-0.5,1,0)
  end)
  segment.s = 20
  transform.copy(segment.lspace, tracktransform)
  return segment
end

do
  --transform.rotate(tracktransform, 0,math.pi * 0.1)
  --transform.rotate(tracktransform, math.pi * 0.1,0)
  local fwd = vec3d(transform.forward(tracktransform)) * 10
  transform.applyTranslation(tracktransform, Vector3D.getArgs(fwd))
  addSegment()
end

setBufferName("track-old.lua")
