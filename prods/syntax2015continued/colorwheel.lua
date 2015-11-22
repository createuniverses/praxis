
do
colorwheelwidget = colorwheelwidget or WidgetLib.newSimple()
colorwheelwidget.width = 256
colorwheelwidget.depth = 256
colorwheelwidget.minx = -256
colorwheelwidget.minz = -256
--transform.scale(colorwheelwidget.lspace, 0.1, 1, 0.1)
--transform.scale(colorwheelwidget.lspace, 100, 1, 100)
end

function makePositionSaver(widgetname)
  local s = [[transform.setTranslation(]]..widgetname..[[.lspace, ]]
  local w = loadstring("return " .. widgetname)
  w = w()
  local x,y,z = transform.getTranslation(w.lspace)
  x = math.floor(x)
  y = math.floor(y)
  z = math.floor(z)
  s = s..x..","..y..","..z..")"
  print2(s)
end

--makePositionSaver("colorwheelwidget")

transform.setTranslation(colorwheelwidget.lspace, 217,-1,44)

glColor = colorGL

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

function angleToColor(a,r,s)
 local red = 255
 local green = 0
 local blue = 0
 if a < 120 then
   red = 255
   if a < 60 then
     blue = linearInterpolate(0, 60, 255, r, a)
     green = r
   else
     blue = r
     green = linearInterpolate(60, 120, r, 255, a)
   end
 elseif a < 240 then
   green = 255
   if a < 180 then
     red = linearInterpolate(120, 180, 255, r, a)
     blue = r
   else
     red = r
     blue = linearInterpolate(180, 240, r, 255, a)
   end
 else
   blue = 255
   if a < 300 then
     green = linearInterpolate(240, 300, 255, r, a)
     red = r
   else
     green = r
     red = linearInterpolate(300, 360, r, 255, a)
   end
 end
 red = red * s
 green = green * s
 blue = blue * s
 return red,green,blue
end

function wrap(n,b)
  local v = ((n-1) % b) + 1
  return v
end

function generateColourInfoTable()
  local names = { "red", "green", "blue" }
  local info = {}
  for i=1,#names,1 do
    local a = (i-1) * 120
    info[names[i]] = {}
    info[names[i]][names[wrap(i-1, #names)]] =
      { min = a+60,
        max = a }
    info[names[i]][names[wrap(i+1, #names)]] =
      { min = a+60,
        max = a+120 }
  end
  return info
end


function colourToAngle(red,green,blue)
  local cols = sortComponents(red,green,blue)
  local info = generateColourInfoTable()

  --print2(inspect(cols))
  --print2(inspect(info))

  local min = cols[3].v
  local max = cols[1].v
  local mid = cols[2].v
  local secinfo = info[cols[1].n][cols[2].n]
  local secmin = secinfo.min
  local secmax = secinfo.max
  local a = linearInterpolate(
                     min,    max,
                     secmin, secmax, mid)
  local r = min
  local s = max / 255
  return a,r,s
end

redslider = redslider or Slider.new(vec3d(0,0,-50), 0, 255)
greenslider = greenslider or Slider.new(vec3d(0,0,-50), 0, 255)
blueslider = blueslider or Slider.new(vec3d(0,0,-50), 0, 255)

continue()

Widgets[6] = nil

6
print2(#Widgets)
3
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

function sortComponents(red,green,blue)
  local cols = { {n = "red",   v = red},
                 {n = "green", v = green},
                 {n = "blue",  v = blue} }
  local getmax = function (from)
    local max = 0
    local maxi = -1
    for i=from,#cols,1 do
      if cols[i].v > max then
        maxi = i
        max = cols[i].v
      end
    end
    return maxi
  end

  local first = getmax(1)
  if first ~= -1 then
    local tmp   = cols[1]
    cols[1]     = cols[first]
    cols[first] = tmp
  end

  local second = getmax(2)
  if second ~= -1 then
    local tmp    = cols[2]
    cols[2]      = cols[second]
    cols[second] = tmp
  end

  return cols
end





