-- widgets.lua

Widgets = {}

WidgetLib = {}

function WidgetLib.new(lspace, width, height, depth)
  local w = {}
  w.lspace = lspace
  w.width = width
  w.height = height
  w.depth = depth
  w.anchored = false  -- I forget the meaning of this
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

function WidgetLib.newSimple()
  local w = {}
  w.lspace = transform.new()
  w.width = 10
  w.height = 10
  w.depth = 10
  w.anchored = false  -- I forget the meaning of this
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

function WidgetLib.addExisting(w)
  if w.lspace == nil then w.lspace = transform.new() end
  if w.width  == nil then w.width = 10 end
  if w.height == nil then w.height = 10 end
  if w.depth == nil then w.depth = 10 end
  if w.anchored == nil then w.anchored = false end
  if w.render == nil then w.render = function (o) end end
  if w.update == nil then w.update = function (o) end end
  if w.lmbdown == nil then w.lmbdown = function (o,x,y,z) end end
  if w.lmbup == nil then w.lmbup = function (o,x,y,z) end end
  if w.rmbdown == nil then w.rmbdown = function (o,x,y,z) end end
  if w.rmbup == nil then w.rmbup = function (o,x,y,z) end end
  if w.mousemove == nil then w.mousemove = function (o,x,y,z) end end
  if tableContains(Widgets, w) == false then
    table.insert(Widgets, w)
  end
end

function WidgetLib.addRender(r)
  local w = {}
  w.lspace = transform.new()
  w.width = 10
  w.height = 10
  w.depth = 10
  w.anchored = false  -- I forget the meaning of this
  w.render = r
  w.update = function (o) end
  w.lmbdown = function (o,x,y,z) end
  w.lmbup = function (o,x,y,z) end
  w.rmbdown = function (o,x,y,z) end
  w.rmbup = function (o,x,y,z) end
  w.mousemove = function (o,x,y,z) end
  table.insert(Widgets, w)
  return w
end

function tableContains(t,item)
  for i=1,#t,1 do
    if t[i] == item then
      return true
    end
  end
  return false
end

function WidgetLib.renderAll()
  for k,v in pairs(Widgets) do
    if v.lspace ~= nil then
      glPushMatrix()
      glApplyTransform(v.lspace)
      v["render"](v)
      -- render bounding box as an option
      glPopMatrix()
    else
      v["render"](v)
    end
  end
end

function WidgetLib.callAll(fnname)
  for k,v in pairs(Widgets) do
    v[fnname](v)
  end
end

function WidgetLib.callAllInRange(fnname)
  local x,y,z = getMouseCursorPos()
  for k,v in pairs(Widgets) do
    if v.rangecheck == nil then
      local lx,ly,lz = transform.globalToLocal(v.lspace, x, y, z)
      if lx > 0 and lx < v.width and
         ly > -v.height and ly < v.height and
         lz > 0 and lz < v.depth then
        v[fnname](v,lx,ly,lz)
      end
    else
      if v.rangecheck(v) then
        v[fnname](v,lx,ly,lz)
      end
    end
  end
end

-- eg.
-- WidgetLib.callAll("update")
-- WidgetLib.callAll("render")
-- WidgetLib.callAllInRange("lmbdown")

