
grid = WidgetLib2.newSimple()

function dcircpts(r,p)
end

function plotloop(l)
  local p1 = l[1]
  for i=2,#l,1 do
    local p2 = l[i]
    drawLine(p1.x, p1.y, p1.z, p2.x, p2.y, p2.z)
    p1 = p2
  end
end

function dcirc(r,p)
  local l = {}
  local s = 360/p
  for a=0,360,s do
    local p = vec3d(r * math.sin(deg2rad(a)),
                    r * math.cos(deg2rad(a)), 0)
    table.insert(l, p)
  end
  return l
end


grid.numsides = 5

grid.hexagon = dcirc(20, grid.numsides)
grid.polygon = dcirc(20, 5)

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

grid.pa = 20

function grid.getpdist()
  local vg = grid.hexagon[2]
  local v1 = vec3d(transform.transformPoint(
    grid.xforms[1],
    Vector3D.getArgs(vg)))
  local v2 = vec3d(transform.transformPoint(
    grid.xforms[2],
    Vector3D.getArgs(vg)))
  
  local d = v2 - v1
  return d:magnitude()
end

function grid.setpa(newpa)
  grid.pa = newpa
end



function grid.updatetrans()
 for i=0,4,1 do
  local r = 20
  --local ra = 120
  local xform = grid.xforms[i]
  transform.copy(xform, transform.identity())
  --xform = transform.identity()
  --grid.xforms[i] = xform

  --glRotate(60*i, 0,0,1)
  --glTranslate(r*math.sin(deg2rad(60)),r*math.cos(deg2rad(60)),0)
  --glRotate(gt*3, r*math.sin(deg2rad(ra)),r*math.cos(deg2rad(ra)),0)
  --glTranslate(0,r,0)


  transform.rotateLocal(xform, deg2rad((360/5)*i + 36), 0, 0, 1)
  transform.translateLocal(xform,
    r*1.5*math.sin(deg2rad(360/5)),
    r*1.5*math.cos(deg2rad(360/5)),
    0)

  --transform.rotateLocal(xform,
  --  deg2rad(0),
  --  math.sin(deg2rad(720/5)),
  --  math.cos(deg2rad(720/5)),
  --  0)

  --transform.translateLocal(xform, 0,r,0)
 end
end

continue()
clearError()

edSetTopMargin(0.5)

function grid.render4(o)
  grid.updatetrans()

  glPushMatrix()
  glTranslate(0,40,0)
  --glRotate(gt * 2, 1,0,0)
  
   gt = gt + 1
   
   local r = 20
   
   glColor(255,255,0,255)
   plotloop(grid.hexagon)
   dcirc(r, 6)
   
   glColor(255,100,0,255)

   local ra = 120

   for i=0,4,1 do
     glPushMatrix()
       glApplyTransform(grid.xforms[i])
       --glRotate(60*i, 0,0,1)
       --glTranslate(r*math.sin(deg2rad(60)),r*math.cos(deg2rad(60)),0)
       --glRotate(gt*3, r*math.sin(deg2rad(ra)),r*math.cos(deg2rad(ra)),0)
       --glTranslate(0,r,0)

       plotloop(grid.hexagon)
       --dcirc(r, 6)
     glPopMatrix()
   end
  glPopMatrix()
end

function grid.render(o)
  grid.pa = grid.pa + 5
  grid.render4(o)
end

clearError()

do
uimainwidget.Widgets = {}
local w = uimainwidget.Widgets
w["grid"] = grid
end












