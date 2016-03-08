
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

showFPS()
setPickSphere(true)
