
function angleToColor(a,r,s)
 local red = 255
 local green = 0
 local blue = 0
 if a < 120 then
   red = 255
   if a < 60 then
     blue = linearInterpolate(0, 60, 255, r, a)
     green = r
   else
     blue = r
     green = linearInterpolate(60, 120, r, 255, a)
   end
 elseif a < 240 then
   green = 255
   if a < 180 then
     red = linearInterpolate(120, 180, 255, r, a)
     blue = r
   else
     red = r
     blue = linearInterpolate(180, 240, r, 255, a)
   end
 else
   blue = 255
   if a < 300 then
     green = linearInterpolate(240, 300, 255, r, a)
     red = r
   else
     green = r
     red = linearInterpolate(300, 360, r, 255, a)
   end
 end
 red = red * s
 green = green * s
 blue = blue * s
 return red,green,blue
end

do
 local sortComponents
 local generateColourInfoTable
 
 function sortComponents(red,green,blue)
  local cols = { {n = "red",   v = red},
                 {n = "green", v = green},
                 {n = "blue",  v = blue} }
  local getmax = function (from)
    local max = 0
    local maxi = -1
    for i=from,#cols,1 do
      if cols[i].v > max then
        maxi = i
        max = cols[i].v
      end
    end
    return maxi
  end

  local first = getmax(1)
  if first ~= -1 then
    local tmp   = cols[1]
    cols[1]     = cols[first]
    cols[first] = tmp
  end

  local second = getmax(2)
  if second ~= -1 then
    local tmp    = cols[2]
    cols[2]      = cols[second]
    cols[second] = tmp
  end

  return cols
 end

 function generateColourInfoTable()
   local names = { "red", "green", "blue" }
   local info = {}
   for i=1,#names,1 do
     local a = (i-1) * 120
     info[names[i]] = {}
     info[names[i]][names[wrap(i-1, #names)]] =
       { min = a+60,
         max = a }
     info[names[i]][names[wrap(i+1, #names)]] =
       { min = a+60,
         max = a+120 }
   end
   return info
 end
 
 function colourToAngle(red,green,blue)
  local cols = sortComponents(red,green,blue)
  local info = generateColourInfoTable()

  --print2(inspect(cols))
  --print2(inspect(info))

  local min = cols[3].v
  local max = cols[1].v
  local mid = cols[2].v
  local secinfo = info[cols[1].n][cols[2].n]
  local secmin = secinfo.min
  local secmax = secinfo.max
  local a = linearInterpolate(
                     min,    max,
                     secmin, secmax, mid)
  local r = min
  local s = max / 255
  return a,r,s
 end
end
