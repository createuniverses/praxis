setBufferName("taskstack.lua")

taskstack = WidgetLib2.newSimple()

taskstack.tasks = {}



function taskstack.render(o)
  glPushMatrix()
  glColor(255,255,255,255)
  glApplyTransform(transform.camera())

  glPushMatrix()
   glTranslate(0.1,0.8,1.1)
   local s = 0.015
   glScale(s*0.7,s,s)
   glRotate(-90, 1,0,0)
   drawText3DStroked("the\ntask\nstack",0,0,0)
  glPopMatrix()

  glPopMatrix()
end
continue()

clearError()

table.insert(Widgets, taskstack)
showError()
print2(getErrorText())
[string "function taskstack.render(o)..."]:11: 4 arguments expected.
stack traceback:
	[string "function onerror(s) endGL() glResetStencil(..."]:1: in function <[string "function onerror(s) endGL() glResetStencil(..."]:1>
	[C]: in function 'drawText3DStroked'
	[string "function taskstack.render(o)..."]:11: in function 'render'
	widgets2.lua:78: in function 'renderWidget'
	widgets2.lua:70: in function 'renderAll'
	widgets.lua:35: in function 'renderAll'
	prod.lua:63: in function 'render'
	[string "render()"]:1: in main chunk

hideError()

print2(getFunction(trace2))
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






do
  closeBuffer()
  switchToBuffer("grid.lua")
end
