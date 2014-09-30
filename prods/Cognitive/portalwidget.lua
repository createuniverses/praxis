clearError()

t2 = transform.cameraBase()
transform.copy(t2, transform.new())
WidgetLib.newRandom()
Widgets = {}

function WidgetLib.newRandom()
  local w = {}
  w.lspace = transform.new()
  local axis = vec3d(math.random(100) - 50,
                   math.random(100) - 50,
                   math.random(100) - 50)
  axis = Vector3D.normalize(axis)
  local angle = math.random(100) * 0.01 * math.pi * 0.5
  transform.rotate(w.lspace, angle, axis.x, axis.y, axis.z)
  transform.setTranslation(w.lspace, math.random(200), math.random(100), math.random(200))
  w.width = 30
  w.height = 10
  w.depth = 50
  w.anchored = false  -- I forget the meaning of this
  w.render = function (o)
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
  end
  w.update = function (o) end
  w.lmbdown = function (o,x,y,z)
    transform.copy(t2, o.lspace)
  end
  w.lmbup = function (o,x,y,z) end
  w.rmbdown = function (o,x,y,z) end
  w.rmbup = function (o,x,y,z) end
  w.mousemove = function (o,x,y,z) end
  table.insert(Widgets, w)
  return w
end

clearError()


