
-- need to make a sandbox first
-- on hold until I make a sandbox
-- done.

x = 5

function vg1()
  return x
end

function ac1(c)
  x = x + c
end

vc = varchangerLib.new(vg1, ac1)

vc.control = -1

for i=1,10,1 do
  print2("x,c = "..x..","..vc.control)
  vc:update()
end

x,c = 5,-1
x,c = 6,1
x,c = 5,-1
x,c = 4,-1
x,c = 3,-1
x,c = 2,-1
x,c = 1,-1
x,c = 0,-1
x,c = -1,-1
x,c = 0,1

grid.pa = 0

grid.setpa(40)
vc2 = varchangerLib.new(grid.getpdist, grid.setpa)

print2(grid.getpdist())
44.982692447186

37.543639671494
clearError()

do
  grid.pa = 0
  vc2.control = 1
end

clearError()

do
  vc2:update()
  print(grid.pa .. " " .. math.floor(grid.getpdist()))
end

print2(getFunction(grid.getpdist))
do
 grid.pa = 0
 function grid.getpdist()
  local vg1 = grid.hexagon[1]
  local vg2 = grid.hexagon[1]
  local v1 = vec3d(transform.transformPoint(
    grid.xforms[1],
    Vector3D.getArgs(vg1)))
  local v2 = vec3d(transform.transformPoint(
    grid.xforms[2],
    Vector3D.getArgs(vg2)))
  
  local d = v2 - v1
  return d:magnitude()
 end
 p
end
clearError()
continue()

function update()
  WidgetLib.callAll("update")
  
  runSchedule()  
  --vc2:update()
  --print(grid.pa .. " " .. math.floor(grid.getpdist()))

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
end



print2(vc2.control)
1

1

-1
print2(grid.pa)
57

39

-1

40

print2(getFunction(grid.setpa))
function grid.setpa(newpa)
  --print("set " .. newpa)
  grid.pa = grid.pa + newpa
end


