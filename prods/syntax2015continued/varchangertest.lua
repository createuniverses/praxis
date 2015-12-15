
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

grid.setpa(40)
vc2 = varchangerLib.new(grid.getpdist, grid.setpa)

vc2.control = 5
clearError()

vc2:update()