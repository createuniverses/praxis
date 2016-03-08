setBufferName("render2.lua")
airplane.followcam = false

a = 0

function render2()
  local v1 = rvec3d(30,deg2rad(a))
  local v2 = rvec3d(30,deg2rad(a+1))
  a = a + 0.1
  drawLine(v1.x,v1.y,v1.z,
           v2.x,v2.y,v2.z)
  
end

dofile("sandbox.lua")

print2(inspect(getmetatable(_G)))
{
  __index = nil --[[<function 1>]],
  __newindex = nil --[[<function 2>]]
}
clearError()





do
  closeBuffer()
  switchToBuffer("widgets.lua - command")
end
