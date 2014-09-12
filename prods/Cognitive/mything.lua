-- mything.lua

clearError()

function mything()
  beginLinGL()
    colorGL(255,255,255,255)
    for i=0,300,10 do
      for j=0,300,10 do
        vectorGL(i,0,j)
        vectorGL(i,math.sin((i * j) * math.pi / 5000) * 50,j)
      end
    end
  endGL()
end

continue()