
function linearInterpolate(inmin, inmax, outmin, outmax, val)
  local proportion = (val - inmin) / (inmax - inmin)
  local out = proportion * (outmax - outmin) + outmin
  return out
end

function lookDown()
  pos = { getCamPos() }
  -- pos[1] = pos[1] + 10 -- look along x
  pos[3] = pos[3] + 10 -- look along z
  lookAt(table.unpack(pos))
  rotateCam(0, math.pi * -0.5)
end

function deg2rad(a)
  return math.pi * (a/180)
end

function rad2deg(a)
  return 180 * (a/math.pi)
end

function table.match(t, ki, vi)
  local results = {}
  function newResult(k, v)
    return 
  end
  for k,v in pairs(t) do
    if v[ki] == vi then
      table.insert(results, {k = k, v = v})
    end
  end
  return results
end

function table.contains(t,item)
  for i=1,#t,1 do
    if t[i] == item then
      return true
    end
  end
  return false
end

function makeCamPosSaver()
  local x,y,z = getCamPos()
  x = math.floor(x)
  y = math.floor(y)
  z = math.floor(z)
  local s = "setCamPos("..x..","..y..","..z..")"
  print2(s)
end

function makePositionSaver(widgetname)
  local s = [[transform.setTranslation(]]..widgetname..[[.lspace, ]]
  local w = loadstring("return " .. widgetname)
  w = w()
  local x,y,z = transform.getTranslation(w.lspace)
  x = math.floor(x)
  y = math.floor(y)
  z = math.floor(z)
  s = s..x..","..y..","..z..")"
  print2(s)
end

function wrap(n,b)
  local v = ((n-1) % b) + 1
  return v
end

glColor = colorGL

function math.atan2p(x,y)
  local a = math.atan2(x,y)
  if a<0 then a = a + math.pi*2 end
  return a
end
