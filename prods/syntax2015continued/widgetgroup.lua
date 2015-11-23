
WidgetGroupLib = {}

function WidgetGroupLib.new(name)
  local w = WidgetLib2.newSimple(name)
  w.Widgets = {}
  w.render = WidgetGroupLib.render
  -- etc  
  return w
end
