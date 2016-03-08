-- widgets.lua

Widgets = {}

WidgetLib = {}

dofile("widgets2.lua")

function WidgetLib.new(lspace, width, height, depth)
  local w = WidgetLib2.new("unnamed", lspace, width, height, depth)
  table.insert(Widgets, w)
  return w
end

function WidgetLib.newSimple()
  local w = WidgetLib2.newSimple("unnamed")
  table.insert(Widgets, w)
  return w
end

function WidgetLib.addExisting(w)
  WidgetLib2.addExisting(w)
  if table.contains(Widgets, w) == false then
    table.insert(Widgets, w)
  end
end

function WidgetLib.addRender(r)
  local w = WidgetLib2.addRender("unnamed", r)
  table.insert(Widgets, w)
  return w
end

function WidgetLib.renderAll()
  WidgetLib2.renderAll(Widgets)
end

function WidgetLib.callAll(fnname)
  WidgetLib2.callAll(Widgets, fnname)
end

function WidgetLib.callAllInRange(fnname)
  local x,y,z = getMouseCursorPos()
  WidgetLib2.callAllInRange(Widgets, fnname, x,y,z)
end


-- eg.
-- WidgetLib.callAll("update")
-- WidgetLib.callAll("render")
-- WidgetLib.callAllInRange("lmbdown")



