function prerender()
end

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

  enableTexturing()
  glBindTexture(t.texId)
  
  if false then

  beginQuadGL()
  colorGL(255,255,255,255)
    local qs = 5
    texGL(0,0) vectorGL(0,0,0)
    texGL(1,0) vectorGL(qs,0,0)
    texGL(1,1) vectorGL(qs,qs,0)
    texGL(0,1) vectorGL(0,qs,0)
  endGL()

  end
  glPopMatrix()



  disableTexturing()
  --glUseProgram(0);
  
  glBindFramebuffer(0);

  glBindTexture(t.texId);
  glGenerateMipmap();
  glBindTexture(0);
end
