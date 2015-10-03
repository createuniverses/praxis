print2(getFunction(edRenderChar))

function edRenderChar(c,n,xp,yp)
  if n==5 then
    drawLine(0,0,10000,10000)
  end
  colorGL(255,0,0)
  edStrokeCharacter(c,0,0)
end

setBufferName("hilitechar.lua")
