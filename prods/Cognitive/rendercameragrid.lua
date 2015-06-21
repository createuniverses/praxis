print2(getFunction(render))

continue()
clearError()

print2(getCamPos())

print2(getErrorText())
[string "function renderCameraGrid()..."]:8: attempt to perform arithmetic on global 'x' (a nil value)
stack traceback:
	[string "function onerror(s) endGL() glResetStencil(..."]:1: in function <[string "function onerror(s) endGL() glResetStencil(..."]:1>
	[string "function renderCameraGrid()..."]:8: in function 'renderCameraGrid'
	[string "function render()..."]:5: in function 'render'
	[string "render()"]:1: in main chunk


function snapNum(n, step)
  local r = n / step
  r = math.floor(r)
  r = r * step
  return r
end

do
  newBuffer()
  for i=-100,100,1.21 do
    print2(i .. ":" .. snapNum(i,10))
  end
end

setBufferName("rendercameragrid.lua")


function renderCameraGrid()
  local cx,cy,cz = getCamPos()
  local h = 1
  local step = 10
  local startx = snapNum(cx-100, 10)
  local startz = snapNum(cz-100, 10)
  for x=startx,startx+200,step do
    drawLine(x,h,cz-100, x,h,cz+100)
  end
  for z=startz,startz+200,step do
    drawLine(cx-100,h,z, cx+100,h,z)
  end
end

function render()
  WidgetLib.renderAll()
  --WidgetLib.callAll("render")

  renderCameraGrid()
    
  renderGears()
  renderGearBots()
  fugue.render()
  
  --renderSlipnet()
  --renderWorkspace()
  
  --testGLColorFunc()
  
  -- colorGL(255,255,0,255)
  
  -- local rpos,wpos,rtot,wtot = getSampleMarkers()
  
  -- drawText2D("" .. rtot, 10,15)
  -- drawText2D("" .. wtot, 10,10)
  
  -- drawText2D("" .. lpfEffNode.samplesLastFrame, 10,5)
  
  SynthNode.renderInputs(sineConNode)
  SynthNode.renderInputs(sineConNode2)
  SynthNode.renderInputs(sineGenNode)
  SynthNode.renderInputs(lpfEffNode)
  SynthNode.renderInputs(sinkNode)
  
  SynthNode.render(lpfEffNode, vec2d(0,0), vec2d(100,30))
  
  --ttestrender()
  
end

