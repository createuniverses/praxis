
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


gt = 0

-- 2d, flat
do
 grid.render = function (o)
   gt = gt + 1
   
   local r = 20
   
   glColor(255,255,0,255)
   dcirc(r, 6)
   
   glColor(255,100,0,255)

   for i=0,5,1 do
     glPushMatrix()
       --local v = rvec3d(r*2,deg2rad(60))
       --v = v * i
       glRotate(60*i, 0,0,1)
       glTranslate(0,r,0)
       glTranslate(r*math.sin(deg2rad(60)),r*math.cos(deg2rad(60)),0)
       dcirc(r, 6)
     glPopMatrix()
   end
end end

do
 grid.render = function (o)
   gt = gt + 1
   
   local r = 20
   
   glColor(255,255,0,255)
   dcirc(r, 6)
   
   glColor(255,100,0,255)

   local ra = 120

   for i=0,0,1 do
     glPushMatrix()
       --glRotate(gt*3, 0,1,0)
       glRotate(gt*3, r*math.sin(deg2rad(ra)),r*math.cos(deg2rad(ra)),0)
       glRotate(60, 0,0,1)
       --glRotate(gt*3, 0,0,1)
       --glRotate(gt*3, 1,0,0)
       --glRotate(gt*3, 1,1,0)
       --glTranslate(0,r,0)
       glTranslate(r*math.sin(deg2rad(60)),r*math.cos(deg2rad(60)),0)
       --glRotate(gt*5, 0,0,1)
       dcirc(r, 6)
     glPopMatrix()
   end
end end

clearError()

do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w["grid"] = grid
end

