Widgets[3].render = function (o)
  
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
  
  -- pressable areas
  glBeginQuads()
    colorGL(255,0,255,255)
    vectorGL(0, 0, 0)
    vectorGL(5, 0, 0)
    vectorGL(5, 0, 5)
    vectorGL(0, 0, 5)
  glEnd()
  
  glBeginQuads()
    colorGL(0,255,0,255)
    vectorGL(5,  0, 0)
    vectorGL(10, 0, 0)
    vectorGL(10, 0, 5)
    vectorGL(5,  0, 5)
  glEnd()
  
  glBuildStencil(0)
  
  o:renderStencil()
  
  glDrawWithinStencil()
  
  glBeginQuads()
    colorGL(255,0,0,255)
    vectorGL(0,       -20, 0)
    vectorGL(o.width, -20, 0)
    vectorGL(o.width, -20, o.depth)
    vectorGL(0,       -20, o.depth)
  glEnd()

  colorGL(0,0,0,255)
  for i=0,o.depth,o.depth/20 do
    drawLine(0,-19,i, o.depth-i,-19,0)
  end
  
  glRemoveStencil()
end

