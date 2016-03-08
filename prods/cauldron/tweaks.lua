
function getPlanePos()
  local pos = vec3d(transform.getTranslation(airplane.lspace))
  return pos
end

function airplane.allstoppilot(o)
  airplane.takeoffpilot(o)
  controls.thrust = 0
end

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


airplane.followcam = true
showdiscs = false
showarms = false
setCamPos(0,35,-30)
lookAt(0,0,50)
airplane.lspace=transform.new()
transform.translate(airplane.lspace, 0,10,0)

maxstreamersegments = 300

showFPS()
setPickSphere(true)

stage = 1
takeoffthrust = 0.75
