setBufferName("slidertest.lua")

print2(getFunction("Widgets[1].render"))

do
Widgets[1].render = function (slider)
  local rpos = linearInterpolate(slider.min, slider.max, 0, slider.depth, slider.pos)
  local mid = slider.width * 0.5
  beginQuadGL()
    colorGL(255,155,0,255)
    vectorGL(0,            0, 0)
    vectorGL(slider.width, 0, 0)
    vectorGL(slider.width, 0, slider.depth)
    vectorGL(0,            0, slider.depth)
    
    colorGL(50,50,50,255)
    vectorGL(mid-5, 1, rpos-5)
    vectorGL(mid+5, 1, rpos-5)
    vectorGL(mid+5, 1, rpos+5)
    vectorGL(mid-5, 1, rpos+5)
  endGL()
  
  colorGL(50,250,50,255)
  glPushMatrix()
  glRotate(-90,0,1,0)
  drawText3DStroked(
    string.format("pos: %.2f,%.2f,%.2f", slider.mousePos.x, slider.mousePos.y, slider.mousePos.z),
    0,0,0)
  glPopMatrix()
  end
end

clearError()
