camwidget = WidgetLib.newSimple("camwidget")


Widgets = {}

Widgets[1] = spirowidget
Widgets[2] = airplane
Widgets[3] = camwidget

print2(#Widgets)

edSetRenderMode(1)

airplane.followcam = false


camwidget.renderbuffer = camwidget.render


print2(getFunction(camwidget.renderbuffer))
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
clearError()

airplane.followcam = true

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

continue()
edSetRenderMode(1)

clearError()

print2(transform.side(transform.camera()))
0.99450886249542
-3.4448302699275e-07
0.10465293377638

-0.10370014607906
-0.13464814424515
0.98546588420868

print2(getFunction("f11Pressed"))
function f11Pressed()
  if mainEditorVisible() then
    hideEditor()
    setKeyRepeat(true)
  else
    showEditor()
  end  
end

mainEditorVisible = editorVisible

function editorVisible() return true end


continue()

print2(inspect(transform))
{
  applyScale = nil --[[<function 1>]],
  applyTranslation = nil --[[<function 2>]],
  camera = nil --[[<function 3>]],
  cameraBase = nil --[[<function 4>]],
  copy = nil --[[<function 5>]],
  forward = nil --[[<function 6>]],
  getScale = nil --[[<function 7>]],
  getTranslation = nil --[[<function 8>]],
  globalToLocal = nil --[[<function 9>]],
  identity = nil --[[<function 10>]],
  localToGlobal = nil --[[<function 11>]],
  lookAt = nil --[[<function 12>]],
  new = nil --[[<function 13>]],
  normalise = nil --[[<function 14>]],
  rotate = nil --[[<function 15>]],
  scale = nil --[[<function 16>]],
  setScale = nil --[[<function 17>]],
  setTranslation = nil --[[<function 18>]],
  side = nil --[[<function 19>]],
  transform = nil --[[<function 20>]],
  translate = nil --[[<function 21>]],
  up = nil --[[<function 22>]]
}


clearError()

do
 local v = vec3d(1,2,3)
 print2(Vector3D.getArgs(v*2))
end
2
4
6

1
2
3

print2(

print2(vec3d(1,2,3).getArgs())
clearError()

clearError()
continue()


--lookAt(0,0,0)


clearError()



print2(inspect(WidgetLib))
{
  addExisting = nil --[[<function 1>]],
  addRender = nil --[[<function 2>]],
  callAll = nil --[[<function 3>]],
  callAllInRange = nil --[[<function 4>]],
  new = nil --[[<function 5>]],
  newSimple = nil --[[<function 6>]],
  renderAll = nil --[[<function 7>]]
}


print2(getFunction(render))
function render()
--  fotsrender()
  WidgetLib.renderAll()

end

function update()
  WidgetLib.callAll("update")
end




setBufferName("camwidget.lua")

print2(getFunction(fotsupdate))
function update()
  WidgetLib.callAll("update")

  if stage == 4 then
    airplane.followcam = false
    airplane.pilot = airplane.allstoppilot
  elseif stage == 3 then
    airplane.followcam = true
    showdiscs = false
    showarms = false
  elseif stage == 2 then
    do
      airplane.followcam = false
      --lookAt(transform.getTranslation(airplane.lspace))
      showdiscs = true
      showarms = true
    end
  elseif stage == 1 then
    airplane.pilot = airplane.normalpilot
  else
    airplane.pilot = airplane.takeoffpilot
  end
  --clearTrace()
  --print(""..getMp3Time())
  
  for i=1,#skythings,1 do
    local thing = skythings[i]
    local planepos = vec3d(transform.getTranslation(airplane.lspace))
    local tween = thing.p - planepos
    local dist = Vector3D.magnitude(tween)
    tween = Vector3D.normalize(tween)
    if dist < (30 + thing.r) then
      thing.p = planepos + (tween * (30+thing.r))
    end
  end
  
  if isMp3Playing() == false then
    os.exit()
  end
end




do
  local name = getParentBufferName()
  closeBuffer()
  switchToBuffer(name)
end



