-- main.lua

--useTonicBases()
--useDominantBases()

function update()
  WidgetLib.callAll("update")
  
  updateGears()
  updateGearBots()
  
  fugue.update()
  
  local bot = gearBots[1]
  if bot ~= nil then 
  local delta = vec2d(math.sin(bot.angle) * 30,
                      math.cos(bot.angle) * 30)
  
  local backpos = bot.pos + delta
  backpos.y = 80
  --setCamPos(bot.pos.x, 180, bot.pos.z)
  --lookAt(backpos.x, backpos.y, backpos.z)
  end
  -- this is now handled in fugue.update()
  --updateSlipnet()
  --updateCoderack()
  

  --local c = (170 * 0.5) + 30
  --orbitCamPP(c,10,c,0.01,0)

  SynthNode.updateSynthNode(sineConNode)
  SynthNode.updateSynthNode(sineConNode2)
  SynthNode.updateSynthNode(sineGenNode)
  SynthNode.updateSynthNode(lpfEffNode)
  SynthNode.updateSynthNode(sinkNode)
  
  -- local rpos,wpos,rtot,wtot = getSampleMarkers()
  
  -- while wtot < rtot + g_samplesPerRequest do
    -- rpos,wpos,rtot,wtot = getSampleMarkers()
    -- --local sample = SynthNode.getSample(sineGenNode)
    -- local sample = SynthNode.getSample(lpfEffNode)
    -- writeSample(sample)
  -- end
  
  g_updateCount = g_updateCount + 1
  
  -- vary bw 0.08 and 0.16
  --fugue.timeBetweenNotes = 0.12 + 0.04 * math.sin(math.pi * 0.015 * g_updateCount)
  --fugue.timeBetweenNotes = 0.12 + 0.04 * math.sin(math.pi * 0.015 * g_updateCount)
  --fugue.timeBetweenNotes = 0.12 + 0.06 * math.sin(math.pi * 0.015 * g_updateCount)
  fugue.timeBetweenNotes = 0.1
  
  --orbitCamPP(60, 20, -120, math.pi * 0.002, 0)
end

function render()
  WidgetLib.renderAll()
  --WidgetLib.callAll("render")
  
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
  
  rendergrid2()
  
  --ttestrender()
  
end

function testGLColorFunc()
  colorGL(255,0,0,255)
  local r,g,b,a = getColorGL()
  local str = r .. "," .. g .. "," .. b .. "," .. a
  drawText3DStroked(str, 10,0,0)
end

function f4Pressed()
  if sl2 == nil then
    sl2 = Slider.new(vec3d(0,0,0), 0.1, 10)
  end

  function sl2.update(slider)
    sineConNode2.freq = slider.pos
  end

  transform.setTranslation(sl2.lspace, Vector3D.getArgs(vec3d(getMouseCursorPos()) + vec3d(0,10,0)))
  transform.rotate(sl2.lspace, math.pi * 0.5, 0)
  --transform.lookAt(sl2.lspace, -100,10,-100)
end

function f3Pressed()
  dofile("composetest.lua")
end

function f4Pressed()
  dofile("scratch.lua")
end

function f5Pressed()
  clearWorkspace()
  activateConcept(slipnet.items["red"])
  activateConcept(slipnet.items["blue"])
end

function f6Pressed()
  playSound()
end

function f7Pressed()
  stopSound()
end

function f9Pressed()
  showEditor()
  newBuffer()
  setBufferText(getErrorText())
end

function f10Pressed()
  showEditor()
  newBuffer()
  setBufferText(getTraceText())
end

function f11Pressed()
  showEditor()
end

function f12Pressed()
  hideEditor()
end

function printToBuffer(...)
  local s = ""
  local args = {...}
  for i=1,#args,1 do
    s = s .. "\n" .. args[i]
  end
  setBufferText(getBufferText() .. "\n" .. s)
  gotoBufferEnd()
end

function OnMouseMove(dx,dy,x,y)
  WidgetLib.callAllInRange("mousemove")
  --print(dx,dy,x,y)
end

function LMBDown(x,y)
  WidgetLib.callAllInRange("lmbdown")
  local x,y,z = getMouseCursorPos()
  --addGear(x,z)
end

function LMBDown(x,y)
  WidgetLib.callAllInRange("lmbdown")
  local x,y,z = getMouseCursorPos()
  selGear = findNearestGear(vec2d(x,z))
  --addGear(x,z)
end

function LMBUp(x,y)
  WidgetLib.callAllInRange("lmbup")
end

function RMBDown(x,y)
  WidgetLib.callAllInRange("rmbdown")
end

function RMBUp(x,y)
  WidgetLib.callAllInRange("rmbup")
end

