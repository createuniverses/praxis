setBufferName("portalwidget-faulttest.lua")

print2(#Widgets)
4

print2(getFunction("Widgets[4].render"))

-- needs to be the last widget in the list
do Widgets[4].render = function (o)
  -- Grid
  glBeginLines()
    colorGL(0,255,0,255)
    for i=0,o.width+1,5 do
      glVertex(i, 0.1, 0)
      glVertex(i, 0.1, o.depth)
    end
    for i=0,o.depth+1,5 do
      glVertex(0,       0.1, i)
      glVertex(o.width, 0.1, i)
    end
  glEnd()
  
  glBuildStencil(0)
  
  o:renderStencil()
  
  -- comment out this line to test leaving opengl
  -- in a bad state
  glDrawWithinStencil()
  
  glBeginQuads()
    colorGL(155,55,255,255)
    vectorGL(0,       -20, 0)
    vectorGL(o.width, -20, 0)
    vectorGL(o.width, -20, o.depth)
    vectorGL(0,       -20, o.depth)
  glEnd()
  
  glRemoveStencil()
end end


