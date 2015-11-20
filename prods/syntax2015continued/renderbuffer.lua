
fotsrender = render
fotsupdate = update
function update()
  --fotsupdate()
end


function render()
--  fotsrender()

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
    setKeyRepeat(true)
  else
    showEditor()
  end  
end

mainEditorVisible = editorVisible

function editorVisible() return true end

