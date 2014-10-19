print2(getMouseCursorPos())
-114.68840026855
-0.14911715686321
-214.83979797363

print2(map)
print2(getFunction("map"))
print2(inspect(map))
nil
clearError()

function map(f,l)
  for i=1,#l,1 do
    l[i] = f(l[i])
  end
end


function f1Pressed()
  local pos = {getMouseCursorPos()}
  map(math.floor, pos)
  pos[2] = pos[2] + 2
  print2(pos[1] .. "," .. pos[2] .. "," .. pos[3])
end
setBufferName("insertmousecoords.lua")

-115,1,-215

-115,-1,-215

-114.68840026855,-0.14911715686321,-214.83979797363

print2(math.floor(2.334343))
2

print2(getFunction("f1Pressed"))
function f1Pressed() luaCall(getBufferText()) end
