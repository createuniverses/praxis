print2(getFunction("render"))
function render()
  WidgetLib.renderAll()
  --WidgetLib.callAll("render")
  
  renderGears()
  renderGearBots()
  fugue.render()
  
  --renderSlipnet()
  --renderWorkspace()
  
  --testGLColorFunc()
  
  -- colorGL(255,255,0,255)
  
  -- local rpos,wpos,rtot,wtot = getSampleMarkers()
  
  -- drawText2D("" .. rtot, 10,15)
  -- drawText2D("" .. wtot, 10,10)
  
  -- drawText2D("" .. lpfEffNode.samplesLastFrame, 10,5)
  
  SynthNode.renderInputs(sineConNode)
  SynthNode.renderInputs(sineConNode2)
  SynthNode.renderInputs(sineGenNode)
  SynthNode.renderInputs(lpfEffNode)
  SynthNode.renderInputs(sinkNode)
  
  SynthNode.render(lpfEffNode, vec2d(0,0), vec2d(100,30))
  
  local lpos = vec3d(0,80,0)
  
  glPushMatrix()
    colorGL(220,120,120,255)
    glTranslate(lpos.x, lpos.y, lpos.z)
    --glScale(10,5,30)
    lightGL(0,0,0)
    glScale(5,5,5)
    glutWireCube(1)
  glPopMatrix()
  
  --lightGL(0,20,0)
  glPushMatrix()
    colorGL(0,120,120,255)
    glTranslate(0,50,0)
    glRotate(lang,0,0,1)
    glScale(10,15,30)
    enableLighting()
      glutSolidCube(1)
    disableLighting()
  glPopMatrix()
  
  --ttestrender()
  lang = lang + 1
end
lang = 0
continue()
clearError()

setBufferName("lighttest.lua")
