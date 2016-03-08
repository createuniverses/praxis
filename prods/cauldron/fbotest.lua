-- praxis:
-- fbotest.lua

dofile("opengl-prelude.lua")

fbotest = {}

function makefbo(w,h, ...)
  local t = {}
  t.w = w
  t.h = h
  t.qs = 100
  
  t.texId = glPrepareFBOTexture(w,h, ...)
  
  t.fboId = glGenFramebuffers()
  glBindFramebuffer(t.fboId)
  
    t.rboId = glGenRenderbuffers()
    glBindRenderbuffer(t.rboId)
      glRenderbufferStorage(w,h)
    glBindRenderbuffer(0)
  
    glFramebufferTexture2D(t.texId)
  glBindFramebuffer(0)
  
  return t
end


--fbotest = makefbo(64,64)
--fbotest = makefbo(128,128)
--fbotest = makefbo(256,256)
fbotest = makefbo(512,512)
--fbotest = makefbo(1024,1024)

-- irregular sizes also work well:
--fbotest = makefbo(131,141)

clearError()

function prerender()
  local t = fbotest
  
  glViewport(0,0,256,256)
  glMatrixModeProjection()
  glLoadIdentity()
  glPerspective(60.0, 1, 1.0, 100.0)
  glMatrixModeModelView()

  glLoadIdentity()
  glTranslate(0, 0, -10)
  
  glBindFramebuffer(t.fboId)

  glClearColor(255, 255, 0, 255)
  glClear()
  
  --glUseProgram(progId)

  colorGL(255,0,0,255)

  glPushMatrix()
  --glRotatef(angle*0.5f, 1, 0, 0);
  --glRotatef(angle, 0, 1, 0);
  --glRotatef(angle*0.7f, 0, 0, 1);
  glTranslate(0, -2, 0);
  --drawTeapot()
  --for i=1,18,1 do
  glutWireSphere(3)
  --end
  glPopMatrix()

  --glUseProgram(0);
  
  glBindFramebuffer(0);

  glBindTexture(t.texId);
  glGenerateMipmap();
  glBindTexture(0);
end

function render()
  WidgetLib.renderAll()

  local h = 5
  local t = fbotest

  enableTexturing()
  glBindTexture(t.texId)

  beginQuadGL()
    texGL(0,0) vectorGL(    0, h,    0)
    texGL(1,0) vectorGL( t.qs, h,    0)
    texGL(1,1) vectorGL( t.qs, h, t.qs)
    texGL(0,1) vectorGL(    0, h, t.qs)
  endGL()

  disableTexturing()

  --renderGreets2()
  --renderskythings()
  trace2()
end

mouseinfo = {}

mouseinfo.clickx = function ()
  local m = mouseinfo
  if m.isdown then return m.down.x else return -m.down.x end
end

mouseinfo.clicky = function ()
  local m = mouseinfo
  if m.isdown then return m.down.y else return -m.down.y end
end

mouseinfo.pos = {x=0,y=0}
mouseinfo.down = {x=0,y=0}
mouseinfo.isdown = false

function LMBDown(x,y)
  local m = mouseinfo
  local mx,my,mz = getMouseCursorPos()
  m.down = {x=mx,y=mz}
  m.isdown = true
  
  --showTrace()
  --print(getMp3Time())
  WidgetLib.callAllInRange("lmbdown")
end

function LMBUp(x,y)
  local m = mouseinfo
  m.isdown = false
  
  WidgetLib.callAllInRange("lmbup")
end

function OnMouseMove(dx,dy,x,y)
  local m = mouseinfo
  --if m.isdown then
    local mx,my,mz = getMouseCursorPos()
    m.pos = {x=mx,y=mz}
  --end
  
  WidgetLib.callAllInRange("mousemove")
end

setCamPos(50,60,50)
lookDown()

disableStdMouseCam()

