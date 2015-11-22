--print2(inspect(sortComponents(1000, 34, 244)))

disableStdMouseCam()
clearError()
clearTrace()

function makeCamPosSaver()
  local x,y,z = getCamPos()
  x = math.floor(x)
  y = math.floor(y)
  z = math.floor(z)
  local s = "setCamPos("..x..","..y..","..z..")"
  print2(s)
end
--makeCamPosSaver()
setCamPos(247,73,72)
lookDown()





do
  closeBuffer()
  switchToBuffer("colorwheel.lua")
end
