-- slider.lua

Slider = {}

function Slider.new(pos, minpos, maxpos)
  local lspace = transform.new()
  transform.setTranslation(lspace, pos.x, pos.y, pos.z)
  transform.lookAt(lspace, pos.x, pos.y, pos.z + 100)
  local slider = WidgetLib.new(lspace, 20,10,100)
  slider.render = Slider.render
  slider.mousemove = Slider.mousemove
  slider.lmbdown = Slider.click
  slider.min = minpos
  slider.max = maxpos
  slider.pos = (minpos + maxpos) * 0.5
  slider.mousePos = vec3d(0,0,0)
  return slider
end

function Slider.render(slider)
  local rpos = linearInterpolate(slider.min, slider.max, 0, slider.depth, slider.pos)
  local mid = slider.width * 0.5
  beginQuadGL()
    colorGL(255,155,0,255)
    vectorGL(0,            0, 0)
    vectorGL(slider.width, 0, 0)
    vectorGL(slider.width, 0, slider.depth)
    vectorGL(0,            0, slider.depth)
    
    local bs = math.min(slider.width, slider.depth)
    bs = bs * 0.3
    colorGL(50,50,50,255)
    vectorGL(mid-bs, 1, rpos-bs)
    vectorGL(mid+bs, 1, rpos-bs)
    vectorGL(mid+bs, 1, rpos+bs)
    vectorGL(mid-bs, 1, rpos+bs)
  endGL()
  
  --colorGL(50,250,50,255)
  --drawText3DStroked(
  --  string.format("pos: %.2f,%.2f,%.2f", slider.mousePos.x, slider.mousePos.y, slider.mousePos.z),
  --  slider.width,0,0)
end

function Slider.mousemove(slider,x,y,z)
  slider.mousePos:set(x,y,z)
  --clearTrace()
  --print(string.format("pos%.2f,%.2f,%.2f", x,y,z))
  
  if getLMBDown() then
    slider.pos = linearInterpolate(0, slider.depth, slider.min, slider.max, z)
  end
end

function Slider.click(slider,x,y,z)
  slider.pos = linearInterpolate(0, slider.depth, slider.min, slider.max, z)
end

