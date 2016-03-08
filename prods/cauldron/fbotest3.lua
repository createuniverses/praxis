-- praxis:

--dofile("mouseshader.lua")
--clearError()

function prerender()
  local t = fbotest
  local u = t.uloc
  local m = mouseinfo
  local w = t.w
  local h = t.h
  local w2 = w*0.5
  local h2 = h*0.5
  
  glViewport(0,0,w,h)
  glMatrixModeProjection()
  glLoadIdentity()
  
  --glPerspective(60.0, 1, 1.0, 100.0)
  --glOrtho(0.0, t.w, 0.0, t.h, 1.0, 100.0)
  glOrtho(-w2, w2, -h2, h2, 1.0, 100.0)
  
  glMatrixModeModelView()

  glLoadIdentity()
  --glTranslate(-t.w*0.5, -t.h*0.5, -50)
  glTranslate(0,0, -50)
  
  glBindFramebuffer(t.fboId)

  glClearColor(1, 0, 255, 255)
  glClear()
  
  glUseProgram(shadprog)
  
  local mfw = t.w/t.qs -- mouse factor for width
  local mfh = t.h/t.qs -- mouse factor for height
  
  glUniformf(u.resolution, t.w, t.h);
  glUniformf(u.mouse, m.pos.x    * mfw, m.pos.y    * mfh,
                      m.clickx() * mfw, m.clicky() * mfh)
  --print(m.pos.x)
  --print(m.pos.y)
  --print(m.clickx())
  --print(m.clicky())
  --glUniformf(u.frame, t.frame)
  
  glPushMatrix()
  
  --glRotatef(angle*0.5f, 1, 0, 0);
  --glRotatef(angle, 0, 1, 0);
  --glRotatef(angle*0.7f, 0, 0, 1);
  
  --glTranslate(0, -2, 0);
  --glutWireSphere(3)

  beginQuadGL()
  colorGL(0, 0, 0, 255)
    --local qs = 5
    --vectorGL(-qs, -qs, 0)
    --vectorGL( qs, -qs, 0)
    --vectorGL( qs,  qs, 0)
    --vectorGL(-qs,  qs, 0)
    do
    local w = t.w * 0.5
    local h = t.h * 0.5
    
    --vectorGL(   0,   0, 0)
    --vectorGL(   w,   0, 0)
    --vectorGL(   w,   h, 0)
    --vectorGL(   0,   h, 0)
    
    vectorGL(  -w,  -h, 0)
    vectorGL(   w,  -h, 0)
    vectorGL(   w,   h, 0)
    vectorGL(  -w,   h, 0)
    end
  endGL()
  
  glUseProgram(0);
  
  if false then
  enableTexturing()
  glBindTexture(t.texId)
  beginQuadGL()
  colorGL(255,255,255,255)
    local qs = 5
    texGL(0,0) vectorGL(0,0,0)
    texGL(1,0) vectorGL(qs,0,0)
    texGL(1,1) vectorGL(qs,qs,0)
    texGL(0,1) vectorGL(0,qs,0)
  endGL()
  disableTexturing()
  end
  glPopMatrix()
  
  glBindFramebuffer(0);

  --glBindTexture(t.texId);
  --glGenerateMipmap();
  glBindTexture(0);
end

