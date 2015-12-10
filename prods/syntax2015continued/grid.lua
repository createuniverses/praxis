
grid = WidgetLib2.newSimple()

function dcirc(r,p)
  local s = 360/p
  local p1 = vec3d(r * math.sin(0),
                   r * math.cos(0), 0)
  for a=s,360,s do
    local p2 = vec3d(r * math.sin(deg2rad(a)),
                     r * math.cos(deg2rad(a)), 0)
    drawLine(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z)
    p1 = p2
  end
end


do
 grid.render = function (o)
   for i=0,5,1 do
     glPushMatrix()
       glTranslate(i*40,i*40,0)
       dcirc(20, 6)
     glPopMatrix()
   end
end end
clearError()

do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w["grid"] = grid
end
