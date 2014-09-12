function WidgetLib.newCamAligned()
  local w = WidgetLib.newSimple()
  w.update = function(w)
    transform.setTranslation(
      w.lspace,
      Vector3D.getArgs(
        vec3d(getCamPos()) + vec3d(getCamFwd()) * -10))
    transform.lookAt(
      w.lspace,
      Vector3D.getArgs(
        vec3d(getCamPos()) + vec3d(getCamFwd()) * 50))
  end
  w.render = function(w)
    beginQuadGL()
      colorGL(255,155,0,255)
      vectorGL(0, 0,0)
      vectorGL(10,0,0)
      vectorGL(10,5,0)
      vectorGL( 0,5,0)
    endGL()
  end
  return w
end

graphWidget = WidgetLib.newCamAligned()

transform.applyTranslation(graphWidget.lspace, 0,10,0)

                          
graphWidget.update = function (w) end


print2(getFunction("graphWidget.update"))

do
  graphWidget.update = function(w)
    transform.setTranslation(
      w.lspace,
      Vector3D.getArgs(
        vec3d(getCamPos()) + vec3d(getCamFwd()) * -10))
    transform.lookAt(
      w.lspace,
      Vector3D.getArgs(
        vec3d(getCamPos()) + vec3d(getCamFwd()) * 50))
    --transform.applyTranslation(w.lspace, 0,5,0)
    local side = vec3d(transform.localToGlobal(w.lspace, 5,2,0))
    side = side - vec3d(transform.getTranslation(w.lspace))
    transform.applyTranslation(w.lspace, Vector3D.getArgs(side))
  end
end













clearError()
continue()








print2(getFPS())



