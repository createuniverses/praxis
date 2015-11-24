
WidgetGroupLib = {}

function WidgetGroupLib.new(name)
  local w = WidgetLib2.newSimple(name)
  w.Widgets = {}
  w.render = function (o) WidgetGroupLib.render(o) end
  -- etc  
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
