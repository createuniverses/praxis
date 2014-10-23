transform.copy(transform.cameraBase(), transform.identity())
portal = WidgetLib.newPortal()
--Widgets = {}

do portal.render = function (o)
  --o.renderStencil(o)
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
  glBeginQuads()
    colorGL(255,155,0,255)
    vectorGL(0,       0, 0)
    vectorGL(o.width, 0, 0)
    vectorGL(o.width, 0, o.depth)
    vectorGL(0,       0, o.depth)
  glEnd()
  glBeginLines()
    colorGL(0,0,0,255)
    for i=0,o.width+1,5 do
      glVertex(i, 0.1, 0)
      glVertex(i, 0.1, o.depth)
    end
    for i=0,o.depth+1,5 do
      glVertex(0,       0.1, i)
      glVertex(o.width, 0.1, i)
    end
  glEnd()
  glDrawWithinStencil()
  glBeginQuads()
    colorGL(255,0,0,255)
    vectorGL(0,       -20, 0)
    vectorGL(o.width, -20, 0)
    vectorGL(o.width, -20, o.depth)
    vectorGL(0,       -20, o.depth)
  glEnd()
  drawLine(0,0,0,100,-100,100)
  glRemoveStencil()
end end
