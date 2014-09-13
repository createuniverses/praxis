-- Name: prod.lua

function moveCamFromBike()
  local cx,cy,cz = getCamPos()
  local fx,fy,fz = getCamFwd()
  local jx,jy,jz = getJoyAxis()
  --jy = math.abs(jy)
  --if math.abs(jy) > 2 then
    cx = cx + fx * math.abs(jy)*jy * -0.00001
    cy = cy + fy * math.abs(jy)*jy * -0.00001
    cz = cz + fz * math.abs(jy)*jy * -0.00001
  --end
  setCamPos(cx,cy,cz)
end

function update()
  moveCamFromBike()
end

function render()
  local jx,jy,jz = getJoyAxis()
  drawText2D("bike: " .. jy,40,90)
end

function lookDown()
  pos = { getCamPos() }
  pos[1] = pos[1] + 10
  lookAt(table.unpack(pos))
  rotateCam(0, math.pi * -0.5)
end

setCamPos(-20,100,20)
lookDown()

-- setMaxFramerate(60)
-- setMaxFramerate(10)
-- setMaxFramerate(20)
setMaxFramerate(50)

windowedMode(0,0,800,600)
--fullscreenMode()
