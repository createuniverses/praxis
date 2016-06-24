
function clamp(x,min,max)
  if x < min then return min end
  if x > max then return max end
  return x
end

updatefns = {}

function update()
  for k,v in pairs(updatefns) do
    v()
  end
end

do updatefns.widget = function ()
  WidgetLib.callAll("update")  
end end

do updatefns.backgroundcol = function ()
  local r,g,b = getClearColor()
  local s = getErrorText()
  if s == "" then
    r = clamp(r-20,0,255)
    g = clamp(g-20,0,255)
    b = clamp(b-20,0,255)
  else
    r = clamp(r-20,90,255)
    g = clamp(g-20,50,255)
    b = clamp(b-20,0,255)
  end
  setClearColor(r,g,b)
end end

do updatefns.skythings = function ()
  for i=1,#skythings,1 do
    local thing = skythings[i]
    local planepos = vec3d(transform.getTranslation(airplane.lspace))
    local tween = thing.p - planepos
    local dist = Vector3D.magnitude(tween)
    tween = Vector3D.normalize(tween)
    if dist < (30 + thing.r) then
      thing.p = planepos + (tween * (30+thing.r))
    end
  end
end end

function addupdatefnhead(fn)
  local oldupdate = update
  update = function ()
    fn()
    oldupdate()
  end
end

function addupdatefntail(fn)
  local oldupdate = update
  update = function ()
    oldupdate()
    fn()
  end
end

function makeupstable(fn)
  local ups = {}
  local d = debug.getinfo(fn)
  for i=1,d.nups,1 do
    local name,val = debug.getupvalue(fn,i)
    ups[name] = val
  end
  return ups
end


function popupdatefn()
  local ups = makeupstable(update)
  update = ups.oldupdate or update
end


