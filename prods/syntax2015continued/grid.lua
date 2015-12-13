
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
function grid.render1(o)
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
end

function grid.render2(o)
   gt = gt + 1
   
   local r = 20
   
   glColor(255,255,0,255)
   dcirc(r, 6)
   
   glColor(255,100,0,255)

   local ra = 120

   for i=0,5,1 do
     glPushMatrix()
       glRotate(60*i, 0,0,1)
       glRotate(gt*3, 0,0,1)
       glRotate(gt*3, 1,0,0)
       --glRotate(gt*3, 1,1,0)

       glTranslate(r*math.sin(deg2rad(60)),r+r*math.cos(deg2rad(60)),0)
       --glTranslate(r*math.sin(deg2rad(60*i)),r+r*math.cos(deg2rad(60*i)),0)
       dcirc(r, 6)
     glPopMatrix()
   end
end

function grid.render3(o)
   gt = gt + 1
   
   local r = 20
   
   glColor(255,255,0,255)
   dcirc(r, 6)
   
   glColor(255,100,0,255)

   local ra = 120

   --for i=0,0,1 do
   do
     local i = 1
     glPushMatrix()
       --glRotate(gt*3, 0,1,0)
       --glRotate(gt*3, r*math.sin(deg2rad(ra)),r*math.cos(deg2rad(ra)),0)
       --glRotate(60, 0,0,1)
       --glRotate(gt*3, 0,0,1)
       --glRotate(gt*3, 1,0,0)
       --glRotate(gt*3, 1,1,0)
       --glTranslate(0,r,0)

       --glRotate(gt * 5, 0,1,0)
       glTranslate(r*math.sin(deg2rad(60)),r*math.cos(deg2rad(60)),0)
       glRotate(gt*3, r*math.sin(deg2rad(ra)),r*math.cos(deg2rad(ra)),0)
       --glRotate(gt*3, 0,1,0)
       glTranslate(0,r,0)
       --glTranslate(r*math.sin(deg2rad(gt*5)),r*math.cos(deg2rad(gt*5)),0)

       --glTranslate(r*math.sin(deg2rad(60)),r+r*math.cos(deg2rad(60)),0)
       --glRotate(gt*5, 0,0,1)
       dcirc(r, 6)
     glPopMatrix()
   end
end

gt = 0

do
 grid.xforms = {}

 for i=0,5,1 do
  local xform = transform.new()
  grid.xforms[i] = xform
 end
end

function transform.rotateLocal(xf, a, vx,vy,vz)
  local v = vec3d(vx,vy,vz)
  local v2 = vec3d(transform.transformVector(xf, v:getArgs()))
  transform.rotate(xf, a, v2:getArgs())
end

function transform.translateLocal(xf, vx,vy,vz)
  local v = vec3d(vx,vy,vz)
  local v2 = vec3d(transform.transformVector(xf, v:getArgs()))
  transform.translate(xf, v2:getArgs())
end

clearError()
continue()

function grid.updatetrans()
 for i=0,5,1 do
  local r = 20
  local ra = 120
  local xform = grid.xforms[i]
  transform.copy(xform, transform.identity())
  --xform = transform.identity()
  --grid.xforms[i] = xform

  --glRotate(60*i, 0,0,1)
  --glTranslate(r*math.sin(deg2rad(60)),r*math.cos(deg2rad(60)),0)
  --glRotate(gt*3, r*math.sin(deg2rad(ra)),r*math.cos(deg2rad(ra)),0)
  --glTranslate(0,r,0)


  transform.rotateLocal(xform, deg2rad(60*i), 0, 0, 1)
  transform.translateLocal(xform,
    r*math.sin(deg2rad(60)),
    r*math.cos(deg2rad(60)),
    0)

  transform.rotateLocal(xform,
    deg2rad(gt*3),
    math.sin(deg2rad(ra)),
    math.cos(deg2rad(ra)),
    0)

  transform.translateLocal(xform, 0,r,0)
 end
end

edSetTopMargin(0.5)

function grid.render4(o)
  grid.updatetrans()

  glPushMatrix()
  glTranslate(0,40,0)
  --glRotate(gt * 2, 1,0,0)
  
   gt = gt + 1
   
   local r = 20
   
   glColor(255,255,0,255)
   dcirc(r, 6)
   
   glColor(255,100,0,255)

   local ra = 120

   for i=0,5,1 do
     glPushMatrix()
       glApplyTransform(grid.xforms[i])
       --glRotate(60*i, 0,0,1)
       --glTranslate(r*math.sin(deg2rad(60)),r*math.cos(deg2rad(60)),0)
       --glRotate(gt*3, r*math.sin(deg2rad(ra)),r*math.cos(deg2rad(ra)),0)
       --glTranslate(0,r,0)

       dcirc(r, 6)
     glPopMatrix()
   end
  glPopMatrix()
end

function grid.render(o)
  grid.render4(o)
end

clearError()

do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w["grid"] = grid
end








