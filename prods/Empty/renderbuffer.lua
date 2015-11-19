function render()
  glPushMatrix()
  glTranslate(0,5,0)
  glScale(0.003, 0.003, 0.003)
  glRotate(90,1,0,0)
  edRenderBuffer()
  glPopMatrix()
end

clearError()

setBufferName("renderbuffer.lua")

print2(getFunction("f11Pressed"))
function f11Pressed()
  if mainEditorVisible() then
    hideEditor()
  else
    showEditor()
  end  
end

mainEditorVisible = editorVisible

function editorVisible() return true end
