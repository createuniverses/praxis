function trace2()
  glPushMatrix()
  glColor(255,255,255,255)
  glApplyTransform(transform.camera())

  glPushMatrix()
   glTranslate(0.5,0.8,1.1)
   local s = 0.015
   glScale(s*0.7,s,s)
   glRotate(-90, 1,0,0)
   drawText3DStroked(
    selectEndLines(getTraceText(),20), 0,0,0)
  glPopMatrix()

  glPushMatrix()
   glTranslate(-1.3,-0.5,1.1)
   local s = 0.015
   glScale(s*0.3,s*0.8,s)
   glRotate(-90, 1,0,0)
   drawText3DStroked(
    selectBeginLines(getErrorText(),5), 0,0,0)
  glPopMatrix()

  glPopMatrix()
end

hideTrace()
hideError()
