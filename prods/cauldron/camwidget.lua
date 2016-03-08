camwidget = WidgetLib.newSimple("camwidget")

Widgets = {}

Widgets[1] = spirowidget
Widgets[2] = airplane
Widgets[3] = camwidget

--edSetRenderMode(1)
--airplane.followcam = false
--airplane.followcam = true

do camwidget.renderbuffer = function (o)
  glPushMatrix()
  glColor(255,255,255,255)
  glPushMatrix()
  local s = 0.0008
  glScale(s, s, s)
  edRenderBuffer()
  glPopMatrix()
  glColor(255,0,0,50)
  glBeginQuads()
    local q2 = 80
    local q1 = 120
    local q3 = 5
    glVertex(0,q3,0)
    glVertex(q1,q3,0)
    glVertex(q1,-q2,0)
    glVertex(0, -q2,0)
  glEnd()
  glPopMatrix()
end end


do camwidget.render = function (o)
  --o.renderbuffer(o)
  glPushMatrix()
  glTranslate(60,-50,40)
  dome.render(dome)
  glPopMatrix()
end end

do camwidget.update = function (o) end
end

do camwidget.update = function (o)
  dome.update(o)
  transform.copy(o.lspace, transform.camera())
  --transform.rotate(
  local fwd = vec3d(transform.forward(o.lspace))
  local side = vec3d(transform.side(o.lspace))
  local up = vec3d(transform.up(o.lspace))
  --transform.translate(o.lspace, 0,-50,0)
  transform.translate(o.lspace, Vector3D.getArgs(fwd * 50))
  transform.translate(o.lspace,
    Vector3D.getArgs(side * -60))
  transform.translate(o.lspace,
    Vector3D.getArgs(up * 40))
end end
