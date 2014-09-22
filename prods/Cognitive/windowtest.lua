clearTrace()

print(getWindowRect())

do
  local x,y,w,h = getWindowRect()
  windowedMode(x,y,700,600)
end

turnOffBorders()
turnOnBorders()


setBufferName("windowtest.lua")


windowedMode(500,200,700,600)


