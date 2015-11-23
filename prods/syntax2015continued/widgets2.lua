
WidgetLib2 = {}

function WidgetLib2.new(name, lspace, width, height, depth)
  local w = {}
  w.lspace = lspace
  w.width = width
  w.height = height
  w.depth = depth
  w.render = function (o) end
  w.update = function (o) end
  w.lmbdown = function (o,x,y,z) end
  w.lmbup = function (o,x,y,z) end
  w.rmbdown = function (o,x,y,z) end
  w.rmbup = function (o,x,y,z) end
  w.mousemove = function (o,x,y,z) end
  return w
end

function WidgetLib2.newSimple(name)
  local w = {}
  w.lspace = transform.new()
  w.width = 10
  w.height = 10
  w.depth = 10
  w.render = function (o) end
  w.update = function (o) end
  w.lmbdown = function (o,x,y,z) end
  w.lmbup = function (o,x,y,z) end
  w.rmbdown = function (o,x,y,z) end
  w.rmbup = function (o,x,y,z) end
  w.mousemove = function (o,x,y,z) end
  return w
end

function WidgetLib.addExisting(w)
  if w.lspace == nil then w.lspace = transform.new() end
  if w.width  == nil then w.width = 10 end
  if w.height == nil then w.height = 10 end
  if w.depth == nil then w.depth = 10 end
  if w.render == nil then w.render = function (o) end end
  if w.update == nil then w.update = function (o) end end
  if w.lmbdown == nil then w.lmbdown = function (o,x,y,z) end end
  if w.lmbup == nil then w.lmbup = function (o,x,y,z) end end
  if w.rmbdown == nil then w.rmbdown = function (o,x,y,z) end end
  if w.rmbup == nil then w.rmbup = function (o,x,y,z) end end
  if w.mousemove == nil then w.mousemove = function (o,x,y,z) end end
end

function WidgetLib2.addRender(name, r)
  local w = {}
  w.lspace = transform.new()
  w.width = 10
  w.height = 10
  w.depth = 10
  w.render = r
  w.update = function (o) end
  w.lmbdown = function (o,x,y,z) end
  w.lmbup = function (o,x,y,z) end
  w.rmbdown = function (o,x,y,z) end
  w.rmbup = function (o,x,y,z) end
  w.mousemove = function (o,x,y,z) end
  return w
end

function WidgetLib2.renderAll(Widgets)
  for k,v in pairs(Widgets) do
    WidgetLib2.renderWidget(v)
  end
end

function WidgetLib2.renderWidget(v)
  if v.lspace ~= nil then
    glPushMatrix()
    glApplyTransform(v.lspace)
    v["render"](v)
    -- render bounding box as an option
    glPopMatrix()
  else
    v["render"](v)
  end
  if v["renderGlobal"] ~= nil then
    v["renderGlobal"](v)
  end
end

function WidgetLib2.callAll(Widgets, fnname)
  for k,v in pairs(Widgets) do
    v[fnname](v)
  end
end

function WidgetLib2.callAllInRange(Widgets, fnname, x,y,z)
  for k,v in pairs(Widgets) do
    if v.rangecheck == nil then
      local lx,ly,lz = transform.globalToLocal(v.lspace, x, y, z)
      v.minx = v.minx or 0
      v.minz = v.minz or 0
      if lx > v.minx and lx < v.width and
         ly > -v.height and ly < v.height and
         lz > v.minz and lz < v.depth then
        v[fnname](v,lx,ly,lz)
      end
    else
      if v.rangecheck(v) then
        v[fnname](v,lx,ly,lz)
      end
    end
  end
end
