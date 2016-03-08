-- turtle.lua
edSetRenderMode(3)
edSetVisLines(30)
edSetVisColumns(60)

turtle = {}
useNamespace(turtle)

turtle.lib = {}
turtle.lib.meta = {__index = lib}

function lib.new()
  local t = {}
  setmetatable(t, lib.meta)
  t.transform = transform.new()
  t.lines = {}
  t.bpendown = true
  return t
end

function lib.forward(t,d)
  local forward = vec3d(t.transform:forward()) * d
  if t.bpendown then
    local v1 = vec3d(t.transform:getTranslation())
    transform.translate(t.transform, forward:getArgs())
    local v2 = vec3d(t.transform:getTranslation())
    local line = {a = v1, b = v2}
    table.insert(t.lines, line)
  else
    transform.translate(t.transform, forward:getArgs())
  end
end

function lib.rotate(t,a)
  transform.rotate(t.transform, deg2rad(a),0,1,0)
end

function lib.penup(t)
  t.bpendown = false
end

function lib.pendown(t)
  t.bpendown = true
end

function lib.render(t)
  local dl = function (a,b)
    drawLine(a.x,2,a.z,b.x,2,b.z)
  end
  local dp = function (a)
    drawLine(a.x,2,a.z,a.x,5,a.z)
  end
  for i=1,#t.lines,1 do
    local line = t.lines[i]
    dl(line.a, line.b)
    dp(line.a)
    dp(line.b)
  end
end

useGlobalNamespace()
