
do
colorwheelwidget = colorwheelwidget or WidgetLib.newSimple()
colorwheelwidget.width = 256
colorwheelwidget.depth = 256
colorwheelwidget.minx = -256
colorwheelwidget.minz = -256
--transform.scale(colorwheelwidget.lspace, 0.1, 1, 0.1)
--transform.scale(colorwheelwidget.lspace, 100, 1, 100)
end

transform.setTranslation(colorwheelwidget.lspace, 217,-1,44)

do
 colorwheelwidget.render = function (o)
   glPushMatrix()
   glScale(0.1, 1, 0.1)
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
   WidgetLib.renderWidget(redslider)
   WidgetLib.renderWidget(greenslider)
   WidgetLib.renderWidget(blueslider)
   --redslider:render()
   --greenslider:render()
   --blueslider:render()
 end
 colorwheelwidget.renderGlobal = function (o)
 end
end

continue()
clearError()

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

redslider = redslider or Slider.new(vec3d(0,0,-50), 0, 255)
greenslider = greenslider or Slider.new(vec3d(0,0,-50), 0, 255)
blueslider = blueslider or Slider.new(vec3d(0,0,-50), 0, 255)

airplane.followcam = false

redslider.render = Slider.render

do
redslider.depth = 100
redslider.width = 10
greenslider.depth = 100
greenslider.width = 10
blueslider.depth = 100
blueslider.width = 10

--makePositionSaver("redslider")
--makePositionSaver("blueslider")
--makePositionSaver("greenslider")

transform.setTranslation(blueslider.lspace, 290,-1,27)
transform.setTranslation(greenslider.lspace, 270,-1,26)
transform.setTranslation(redslider.lspace, 252,-1,27)

transform.setTranslation(redslider.lspace, 20,0,0)
transform.setTranslation(greenslider.lspace, 40,0,0)
transform.setTranslation(blueslider.lspace, 60,0,0)
end


--transform.setTranslation(colorwheelwidget.lspace, getMouseCursorPos())
--transform.setTranslation(redslider.lspace, getMouseCursorPos())
--transform.setTranslation(greenslider.lspace, getMouseCursorPos())
--transform.setTranslation(blueslider.lspace, getMouseCursorPos())
