setBufferPosY(60480)



lookDown()
turnOnBorders()
turnOffBorders()

print(getBufferBB())


do
clearTrace()
local minx,miny,maxx,maxy = getBufferBB()
print(minx,miny,maxx,maxy)
print((miny+maxy)/2)
print((maxy-miny)/2 - maxy)
end


setBufferName("buffcmdtest.lua")


0


