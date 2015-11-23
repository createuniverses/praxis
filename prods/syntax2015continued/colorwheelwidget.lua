
do
colorwheelwidget = colorwheelwidget or WidgetLib2.newSimple("colorwheel")
colorwheelwidget.width = 256
colorwheelwidget.depth = 256
colorwheelwidget.minx = -256
colorwheelwidget.minz = -256

colorwheelgrp = colorwheelgrp or WidgetGroupLib.new("colorwheelgrp")
colorwheelgrp.Widgets[1] = colorwheelwidget
end

continue()

do
colorwheelwidget.lspace = transform.new()
transform.scale(colorwheelwidget.lspace, 0.1, 0.1, 1)
end

transform.setTranslation(colorwheelwidget.lspace, 0,0,0)

colorwheelwidget.lspace = transform.new()

do
 colorwheelwidget.render = function (o)
   glPushMatrix()
   glRotate(90, 1,0,0)
   --glScale(0.1, 1, 0.1)
   glColor(200,100,100,255)
   glBeginTriangles()
    local step = 10
    for a = 0,360-step,step do
     local r = 256
     local x1 = math.sin(deg2rad(a)) * r
     local y1 = math.cos(deg2rad(a)) * r
     local x2 = math.sin(deg2rad(a+step)) * r
     local y2 = math.cos(deg2rad(a+step)) * r
     glColor(255,255,255,255)
     glVertex(0,0,0)
     local red,green,blue = angleToColor(a,0,1)
     glColor(red, green, blue)
     glVertex(x1,0,y1)
     glColor(red, green, blue)
     glVertex(x2,0,y2)
    end
   glEnd()
   
   glColor(redslider.pos, greenslider.pos, blueslider.pos, 255)
   glPushMatrix()
   glTranslate(0,0,-400)
   glBeginQuads()
     glVertex(0,0,0)
     glVertex(100,0,0)
     glVertex(100,0,100)
     glVertex(0,0,100)
   glEnd()
   glPopMatrix()
   
   do
    local a,r,s = colourToAngle(redslider.pos, greenslider.pos, blueslider.pos)
    local x = math.sin(deg2rad(a)) * (256-r)
    local z = math.cos(deg2rad(a)) * (256-r)
    glColor(0,0,0,255)
    local h = 0.5
    drawLine(x-25, h, z, x+25, h, z)
    drawLine(x, h, z-25, x, h, z+25)
    glColor(255,255,255,255)
    
   end
   glPopMatrix()
   --WidgetLib.renderWidget(redslider)
   --WidgetLib.renderWidget(greenslider)
   --WidgetLib.renderWidget(blueslider)
   --redslider:render()
   --greenslider:render()
   --blueslider:render()
 end
 colorwheelwidget.renderGlobal = function (o)
 end
end


function math.atan2p(x,y)
  local a = math.atan2(x,y)
  if a<0 then a = a + math.pi*2 end
  return a
end

do
 colorwheelwidget.lmbdown = function (o,x,y,z)
  local r = 256 - math.sqrt(x*x + z*z)
  local a = rad2deg(math.atan2p(x,z))
  local r,g,b = angleToColor(a,r,1)
  --print(r..","..g..","..b)
  redslider.pos = r
  greenslider.pos = g
  blueslider.pos = b
 end

 function colorwheelwidget.mousemove(o,x,y,z)
  if getLMBDown() then
   local r = 256 - math.sqrt(x*x + z*z)
   local a = rad2deg(math.atan2p(x,z))
   local r,g,b = angleToColor(a,r,1)
   --print(r..","..g..","..b)
   redslider.pos = r
   greenslider.pos = g
   blueslider.pos = b
  end
 end
end


airplane.followcam = false

redslider.render = Slider.render

do
end
continue()

-- setting position according to mouse
--transform.setTranslation(colorwheelwidget.lspace, getMouseCursorPos())
--transform.setTranslation(redslider.lspace, getMouseCursorPos())
--transform.setTranslation(greenslider.lspace, getMouseCursorPos())
--transform.setTranslation(blueslider.lspace, getMouseCursorPos())

do colorwheelgrp.update = function (o)
end end

colorwheelgrp.lspace = transform.new()
colorwheelwidget.lspace = transform.new()
transform.copy(colorwheelgrp.lspace, transform.camera())

colorwheelgrp.render = function (o) WidgetGroupLib.render(o) end


do colorwheelgrp.render = function (o)
  --o.renderbuffer(o)
  glPushMatrix()
  glApplyTransform(colorwheelwidget.lspace)
  glTranslate(60,-50,40)
  colorwheelwidget.render(colorwheelwidget)
  dome.render(dome)
  glPopMatrix()
end end

do colorwheelgrp.update = function (o)
  --dome.update(o)
  transform.copy(o.lspace, transform.camera())
end end

do colorwheelgrp.update = function (o)
  --dome.update(o)
  transform.copy(o.lspace, transform.camera())
  local fwd = vec3d(transform.forward(o.lspace))
  local side = vec3d(transform.side(o.lspace))
  local up = vec3d(transform.up(o.lspace))
  
  transform.translate(o.lspace, 0,0,0)

  transform.translate(o.lspace,
    Vector3D.getArgs(fwd * 70))
  transform.translate(o.lspace,
    Vector3D.getArgs(side * 0))
  transform.translate(o.lspace,
    Vector3D.getArgs(up * 0))
end end

Widgets[4] = nil
print2(#Widgets)
3
clearError()
continue()
Widgets = {}

Widgets[1] = spirowidget
Widgets[2] = colorwheelgrp

print2(#colorwheelgrp.Widgets)
1
colorwheelgrp.Widgets = {}
colorwheelgrp.Widgets[1] = colorwheelwidget

