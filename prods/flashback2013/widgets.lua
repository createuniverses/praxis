-- widgets.lua

Widgets = {}

WidgetLib = {}

function WidgetLib.new(lspace, width, height, depth)
  local w = {}
  w.lspace = lspace
  w.width = width
  w.height = height
  w.depth = depth
  w.anchored = false
  w.render = function (o) end
  w.update = function (o) end
  w.lmbdown = function (o,x,y,z) end
  w.lmbup = function (o,x,y,z) end
  w.rmbdown = function (o,x,y,z) end
  w.rmbup = function (o,x,y,z) end
  w.mousemove = function (o,x,y,z) end
  table.insert(Widgets, w)
  return w
end

function WidgetLib.callAll(fnname)
  for k,v in pairs(Widgets) do
    v[fnname](v)
  end
end

function WidgetLib.callAllInRange(fnname)
  local x,y,z = getMouseCursorPos()
  for k,v in pairs(Widgets) do
    local lx,ly,lz = transform.globalToLocal(v.lspace, x, y, z)
    if lx > 0 and lx < v.width and
       ly > -v.height and ly < v.height and
       lz > 0 and lz < v.depth then
      v[fnname](v,lx,ly,lz)
    end
  end
end

-- eg.
-- WidgetLib.callAll("update")
-- WidgetLib.callAll("render")
-- WidgetLib.callAllInRange("lmbdown")
