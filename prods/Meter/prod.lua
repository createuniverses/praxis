-- Name: prod.lua

dofile("geometry.lua")
dofile("drawing.lua")

meter = texture.new()
--texture.clear(meter) -- not necessary
--texture.setRenderFn(meter, "drawMeter()")

function drawMeter()
    glMatrixModeProjection()
    glPushMatrix()
    glLoadIdentity()
    glOrtho(0,100,0,100,0,10)

    glMatrixModeModelView()
    glPushMatrix()
    glLoadIdentity();
    
    --drawText2D("hello",50,50)
    
    local center = vec3d(50,50,0)

    for i=0,math.pi * 0.25,math.pi * 0.02 do
      local pos1 = center + (vec3d(math.sin(i),math.cos(i), 0) * 40)
      local pos2 = center + (vec3d(math.sin(i),math.cos(i), 0) * 30)
      drawLine(pos1.x,pos1.y,0,pos2.x, pos2.y,0)

      local pos1 = center + (vec3d(math.sin(-i),math.cos(-i), 0) * 40)
      local pos2 = center + (vec3d(math.sin(-i),math.cos(-i), 0) * 30)
      drawLine(pos1.x,pos1.y,0,pos2.x, pos2.y,0)
    end
    
    --drawLine(math.random(100),math.random(100),0,math.random(100),math.random(100),0)
    
    glMatrixModeModelView()
    glPopMatrix()
    glMatrixModeProjection()
    glPopMatrix()
end


function update()
end

function render()
  --texture.beginDrawing(meter)
  --drawMeter()
  --texture.endDrawing(meter)
  
  --texture.update(meter)
  
  colorGL(255,255,255,255)
  drawTexQuad(vec3d(0, 20, 0), 300)
end

function lookDown()
  pos = { getCamPos() }
  pos[1] = pos[1] + 10
  lookAt(table.unpack(pos))
  rotateCam(0, math.pi * -0.5)
end

--setCamPos(50,100,50)
setCamPos(-20,100,20)
lookDown()

function getFunction(fnname)
  local dt = debug.getinfo(
    load("return " .. string.gsub(fnname, ":", "."))())
  if string.sub(dt.source, 1,1) == "@" then
    --print(dt.short_src)
    --print(dt.linedefined, dt.lastlinedefined)
    local filetext = readFile(dt.short_src)
    local fntext = selectLines(filetext, dt.linedefined, dt.lastlinedefined)
    --print(fntext)
    return fntext
  else
    local fntext = dt.source
    --print(fntext)
    return fntext
  end
end

function editfn(fnname)
  local fntxt = getFunction(fnname)
  newBuffer()
  setBufferText(fntxt)
end

windowedMode()

loadBuffer("meterdraw.lua")

--editfn("render")
