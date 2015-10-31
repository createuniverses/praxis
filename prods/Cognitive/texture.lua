-- Name: prod.lua

dofile("geometry.lua")
dofile("drawing.lua")
dofile("reflect.lua")

textures = {}

for i=1,5,1 do
  textures[i] = texture:new()
  texture.clear(textures[i])
  texture.setRenderFn(textures[i], "textureRenderFn("..i..")")
end

function clearTextures()
  for i=1,#textures,1 do
    texture.clear(textures[i])
  end
end

function textureRenderFn(i)
    glMatrixModeProjection()
    glPushMatrix()
    glLoadIdentity()
    glOrtho(0,400,0,400,0,10)

    glMatrixModeModelView()
    glPushMatrix()
    glLoadIdentity();
    
    drawText3D("hello" .. i,
       math.random(20) + 40,math.random(20) + 40,0)
    drawLine(0,0,0,200,100,0)

    do
      texture.select(textures[2])
      colorGL(255,255,255,255)
      drawTexQuad2(vec3d(200, 200, -9), 200)
    end

    glMatrixModeModelView()
    glPopMatrix()
    glMatrixModeProjection()
    glPopMatrix()
end

clearError()
clearTrace()
hideEditor()

do
  local i = 0
  function renderTextures()
    texture.update(textures[i+1])
    for i=1,#textures,1 do
      --texture.update(textures[i])
      texture.select(textures[i])
      colorGL(255,255,255,255)
      drawTexQuad(vec3d((i-1) * 60, 20, 0), 50)
    end
    i = (i + 1) % #textures
  end
end

do
  function renderTextures()
    texture.update(textures[1])
    --texture.update(textures[2])
    texture.select(textures[1])
    colorGL(255,255,255,255)
    drawTexQuad(vec3d(0, 20, 0), 50)
  end
end

function update()
end

function render()
  renderTextures()
end

function lookDown()
  pos = { getCamPos() }
  pos[1] = pos[1] + 10
  lookAt(table.unpack(pos))
  rotateCam(0, math.pi * -0.5)
  rotateCam(math.pi * -0.5, 0)
end

setCamPos(-20,100,20)
lookDown()

-- setMaxFramerate(60)
-- setMaxFramerate(10)
-- setMaxFramerate(20)
--setMaxFramerate(50)

--windowedMode()
--fullscreenMode()

--windowedMode(0,0, getWinScreenWidth() * 0.5, getWinScreenHeight())

windowedMode(0,0,800,600)

