do
redslider = nil
greenslider = nil
blueslider = nil
end

do
redslider = redslider or Slider.new(vec3d(0,0,-50), 0, 255)
greenslider = greenslider or Slider.new(vec3d(0,0,-50), 0, 255)
blueslider = blueslider or Slider.new(vec3d(0,0,-50), 0, 255)
end


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

colorwheelgrp.Widgets = {}

colorwheelgrp.Widgets[1] = redslider
colorwheelgrp.Widgets[1] = colorwheelwidget

continue()

print2(getFunction(redslider.render))

print2(redslider.depth)

function redslider.render(slider)
  glScale(0.1, .1, 0.1)
  colorwheelwidget.render(colorwheelwidget)
end

print2(getFunction(colorwheelwidget.render))
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
end


function redslider.render(slider)
   glPushMatrix()
   glRotate(270, 1,0,0)
  local rpos = linearInterpolate(slider.min, slider.max, 0, slider.depth, slider.pos)
  local mid = slider.width * 0.5
  beginQuadGL()
    colorGL(255,155,0,255)
    vectorGL(0,            0, 0)
    vectorGL(slider.width, 0, 0)
    vectorGL(slider.width, 0, slider.depth)
    vectorGL(0,            0, slider.depth)
    
    local bs = math.min(slider.width, slider.depth)
    bs = bs * 0.3
    colorGL(50,50,50,255)
    vectorGL(mid-bs, 1, rpos-bs)
    vectorGL(mid+bs, 1, rpos-bs)
    vectorGL(mid+bs, 1, rpos+bs)
    vectorGL(mid-bs, 1, rpos+bs)
  endGL()
  
  --colorGL(50,250,50,255)
  --drawText3DStroked(
  --  string.format("pos: %.2f,%.2f,%.2f", slider.mousePos.x, slider.mousePos.y, slider.mousePos.z),
  --  slider.width,0,0)
  glPopMatrix()
end



print2(#colorwheelgrp.Widgets)

redslider.lspace = transform.new()
transform.setTranslation(redslider.lspace, 0,0,0)

