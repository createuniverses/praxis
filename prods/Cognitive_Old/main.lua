-- Name: main.lua

updateNum = 0

function update()
  if #coderack == 0 then
    generateBottomUpCodelets()
  end
  
  updateWorkspace()
  updateCoderack()
  updateSlipnet()
  
  if updateNum % 10 == 0 then
    --updateCoderack()
  end
  
  updateNum = updateNum + 1
end

dbgPos = { x = -10, y = 0, z = -10 }

function render()
  disablePolygonOffset()
  --setPolygonOffset(30,1)
  
  renderWorkspace()
  renderSlipnet()
  renderCoderack()
  
  -- render debug position
  setColorGL(255,255,0,255)
  drawPolygon({x = dbgPos.x, y = dbgPos.y, z = dbgPos.z},7,4,math.pi/4,3)
  
end


--setCamPos(50,100,50)
setCamPos(-20,100,20)
lookDown()
-- setMaxFramerate(60)
-- setMaxFramerate(10)
-- setMaxFramerate(20)
setMaxFramerate(50)

windowedMode()
--fullscreenMode()
