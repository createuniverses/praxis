
dofile("cwsliders.lua")

do
colorwheelwidget = WidgetLib2.newSimple("colorwheel")
colorwheelwidget.width = 256
colorwheelwidget.depth = 256
colorwheelwidget.minx = -256
colorwheelwidget.minz = -256

colorwheelwidget.lspace = transform.new()
transform.scale(colorwheelwidget.lspace, 0.1, 1, 0.1)
transform.rotate(colorwheelwidget.lspace, deg2rad(-90), 1,0,0)

colorwheelgrp = colorwheelgrp or WidgetGroupLib.new("colorwheelgrp")
colorwheelgrp.Widgets[1] = colorwheelwidget

colorwheelgrp.render = function (o) WidgetGroupLib.render(o) end

showCWWithSliders()
end

do
 colorwheelwidget.render = function (o)
   glPushMatrix()
   --glRotate(90, 1,0,0)
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
     local red2,green2,blue2 = angleToColor(a+step,0,1)
     glColor(red2, green2, blue2)
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
 end
 
 colorwheelwidget.renderGlobal = function (o)
 end
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

function colorwheelgrp.rangecheck(o, x, y, z)
  if z < 5 and z > -5 then
    return true
  else
    return false
  end
end

function colorwheelgrp.mousemove(o,x,y,z)
  WidgetLib2.callAllInRange(o.Widgets, "mousemove", x,y,z)
  if getLMBDown() then
--   print("mm with md")
  else
--   print("mm only "
--    ..math.floor(x)..","
--    ..math.floor(y)..","
--    ..math.floor(z))
  end
end

function colorwheelgrp.lmbdown(o,x,y,z)
  WidgetLib2.callAllInRange(o.Widgets, "lmbdown", x,y,z)
end

disableStdMouseCam()


do colorwheelgrp.update = function (o)
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
