
WidgetGroupLib = {}

function WidgetGroupLib.new(name)
  local w = WidgetLib2.newSimple(name)
  w.Widgets = {}
  w.render = WidgetGroupLib.render
  -- etc  
  return w
end

function WidgetGroupLib.render(w)
  -- render each of the items in Widgets
  -- after first applying our transform.
end
