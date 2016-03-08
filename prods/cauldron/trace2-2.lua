print2(getFunction(render))
function render()
  WidgetLib.renderAll()

  --renderGreets2()
  --renderskythings()
  trace2()
end
showTrace()
hideTrace()

setBufferName("trace2-2.lua")

print2(getFunction(trace2))
function trace2()
  glPushMatrix()
  glColor(255,255,255,255)
  glApplyTransform(transform.camera())

  glPushMatrix()
   glTranslate(0.5,0.8,1.1)
   local s = 0.015
   glScale(s*0.4,s*0.8,s)
   glRotate(-90, 1,0,0)
   --drawText3D("wot\nwot\nwot", 0,0,0)
   drawText3DStroked(
   --drawText3D(
    selectEndLines(getTraceText(),20), 0,0,0)
  glPopMatrix()

  glPushMatrix()
   glTranslate(0.1,-0.5,1.1)
   local s = 0.015
   glScale(s*0.2,s*0.4,s)
   glRotate(-90, 1,0,0)
   --drawText2D("testing")
   drawText3DStroked(
    selectBeginLines(getErrorText(),5), 0,0,0)
  glPopMatrix()

  glPopMatrix()
end
continue()


for i=1,10,1 do
print("blingy")
print("blah")
end
hideTrace()



showTrace()
showError()
hideError()

clearError()
edSet()







do
  closeBuffer()
  switchToBuffer("prod.lua")
end
