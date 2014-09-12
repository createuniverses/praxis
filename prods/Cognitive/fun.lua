t = 0

function render()
  local px = 0
  local py = 0
  for i=0,math.pi * 4, math.pi * 0.1 do
    x = i * 50
    y = math.sin(i * (1.2 + math.sin(t))) * 50
    drawLine(x,0,y,px,0,py)
    px = x
    py = y
  end
  t = t + math.pi * 0.02
end

--setBufferName("fun.lua")