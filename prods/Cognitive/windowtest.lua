clearTrace()
print(getWindowRect())

windowedMode(0,0,700,600)
windowedMode(200,100,700,600)

do
  local x,y,w,h = getWindowRect()
  windowedMode(x,y,700,600)
  clearTrace()
  print(getWindowRect())
end

turnOffBorders()
turnOnBorders()

setBufferName("windowtest.lua")
