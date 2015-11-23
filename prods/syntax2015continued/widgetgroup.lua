
WidgetGroupLib = {}

function WidgetGroupLib.new(name)
  local w = WidgetLib2.newSimple(name)
  w.Widgets = {}
  w.render = function (o) WidgetGroupLib.render(o) end
  -- etc  
  return w
end

function WidgetGroupLib.render(w)
  -- render each of the items in Widgets
  -- after first applying our transform.

  -- the parent transform has already been applied
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
    --print("yi")
    glPushMatrix()
    glApplyTransform(v.lspace)
    v["render"](v)
    -- render bounding box as an option
    glPopMatrix()
  else
    v["render"](v)
  end
end


