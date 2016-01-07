do
 local gridbak = swapoutgrid()
 
 local grid = {}
 
 getmygrid = function (x,y)
   grid[x] = grid[x] or {}
   return grid[x][y] or 10
 end
 setmygrid = function (x,y,v)
   grid[x] = grid[x] or {}
   grid[x][y] = v
 end

 local gg = getmygrid

 function swapingrid(g)
   grid = g
 end
 
 function swapoutgrid()
  return grid
 end

 function rendergrid2()
  local s = 5
  local h = 10
  for x = -100,100,s do
    for y = -100,100,s do
      h1 = gg(x,y)
      h2 = gg(x+s,y)
      h3 = gg(x,y+s)
      drawLine(x,h1,y,x+s,h2,y)
      drawLine(x,h1,y,x,  h3,y+s)
    end
  end
 end
 
 swapingrid(gridbak)

 clearError()
 continue()
end


function rendergrid2() end

for i=0,90,5 do
 setmygrid(i,0,70)
end

function applygridaveraging()
 local gg = getmygrid
 local s = 5
 for x=-50,50,s do
   for y=-50,50,s do
     local h1 = gg(x,  y)
     local h2 = gg(x+s,y)
     local h3 = gg(x+s,y+s)
     local h4 = gg(x  ,y+s)
     local h = gg(x,y)
     local avg = (h1+h2+h3+h4)/4
     h = h + (avg - h) * 0.1
     --h = avg
     --h = 5
     setmygrid(x,y,h)
   end
 end
end

clearTrace()

for i=1,20,1 do
  local r = 20
  local x = (math.random(r)-r/2) * 5
  local y = (math.random(r)-r/2) * 5
  local z = math.random(90) + 5
  setmygrid(x,y,z)
end

do
 setmygrid(0,0,50)
 setmygrid(0,5,50)
 setmygrid(0,10,50)
 setmygrid(0,15,50)
end

clearError()
continue()
setBufferName("griddy.lua")

_gridbak = swapoutgrid()
print2(_gridbak[0][0])
swapingrid(_gridbak)



continue()
print2(getErrorText())






do
  closeBuffer()
  switchToBuffer("synth.lua - command")
end

print2(getFunction(update))
function update()
  WidgetLib.callAll("update")

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

  local r,g,b = getClearColor()
  r = r - 10
  if r < 0 then r = 0 end
  setClearColor(r,g,b)

  SynthNode.updateSynthNode(sineConNode)
  SynthNode.updateSynthNode(sineConNode2)
  SynthNode.updateSynthNode(sineGenNode)
  SynthNode.updateSynthNode(lpfEffNode)
  SynthNode.updateSynthNode(sinkNode)

 --applygridaveraging()
end
