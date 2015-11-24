
WidgetGroupLib = {}

function WidgetGroupLib.new(name)
  print("WidgetGroupLib.new")
  local w = WidgetLib2.newSimple(name)
  w.Widgets = {}
  w.render = function (o) WidgetGroupLib.render(o) end
  -- etc  
  print(name)
  print("w.name:" .. w.name)
  return w
end

function WidgetGroupLib.render(w)
  for k,v in pairs(w.Widgets) do
    WidgetGroupLib.renderWidget(v)
  end

  for k,v in pairs(w.Widgets) do
    if v["renderGlobal"] ~= nil then
      v["renderGlobal"](v)
    end
  end
end


function WidgetGroupLib.renderWidget(v)
  if v.lspace ~= nil then
    glPushMatrix()
    glApplyTransform(v.lspace)
    v["render"](v)
    glPopMatrix()
  else
    v["render"](v)
  end
end

function WidgetGroupLib.update_cam(o)
  transform.copy(o.lspace, transform.camera())
  local fwd = vec3d(transform.forward(o.lspace))
  local side = vec3d(transform.side(o.lspace))
  local up = vec3d(transform.up(o.lspace))
  
  transform.translate(o.lspace, 0,0,0)

  transform.translate(o.lspace,
    Vector3D.getArgs(fwd * 70))
  transform.translate(o.lspace,
    Vector3D.getArgs(side * 0))
  transform.translate(o.lspace,
    Vector3D.getArgs(up * 0))
end

function WidgetGroupLib.rangecheck_flat(o, x, y, z)
  if z < 5 and z > -5 then
    return true
  else
    return false
  end
end

function WidgetGroupLib.mousemove(o,x,y,z)
  WidgetLib2.callAllInRange(o.Widgets, "mousemove", x,y,z)
end

function WidgetGroupLib.lmbdown(o,x,y,z)
  WidgetLib2.callAllInRange(o.Widgets, "lmbdown", x,y,z)
end
