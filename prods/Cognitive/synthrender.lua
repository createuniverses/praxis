print2(getFunction(render))
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
  
  --ttestrender()
  
end

print2(getFunction(SynthNode.renderInputs))
function SynthNode.renderInputs(node)
  for k,v in pairs(node.inputs) do
    SynthNode.render(v, v.pos, node.pos)
  end
end


print2(getFunction(SynthNode.render))
function SynthNode.render(node, from, to)
  local trans = makeSynthTrans(from, to, SynthNode.rbuffsize)
  glPushMatrix()
  glApplyTransform(trans)
  beginLinGL()
  colorGL(255,255,255,255)
  for i=1,SynthNode.rbuffsize - 1,1 do
    local s1 = node.rbuffer[i]
    local s2 = node.rbuffer[i+1]
    
    --vectorGL(transform.localToGlobal(trans, s1,1,-i))
    --vectorGL(transform.localToGlobal(trans, s2,1,-i-1))
    
    vectorGL(s1,1,i)
    vectorGL(s2,1,i)
    vectorGL(s2,1,i)
    vectorGL(s2,1,i+1)
    
    --vectorGL(transform.localToGlobal(trans, s1,1,i))
    --vectorGL(transform.localToGlobal(trans, s2,1,i))
    --vectorGL(transform.localToGlobal(trans, s2,1,i))
    --vectorGL(transform.localToGlobal(trans, s2,1,i+1))
  end
  endGL()
  glPopMatrix()
end

setBufferName("synthrender.lua")

print2(getFunction(update))
function update()
  WidgetLib.callAll("update")
  
  if coroutine.status(initServerRoutine) ~= "dead" then
    coroutine.resume(initServerRoutine)
  else
    svrRunPromptServer(sck1)
    svrRunEchoServer(sck2)
  end
  
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

print2(getFunction(SynthNode.updateSynthNode))
function SynthNode.updateSynthNode(node)
  SynthNode[node.updateFn](node)
  SynthNode.makeRBuffer(node)
end

print2(getFunction(SynthNode.makeRBuffer))
function SynthNode.makeRBuffer(node)
  if node.buffer == nil then return end
  local bindex = 1
  for i=1,SynthNode.rbuffsize,1 do
    if bindex <= Queue.size(node.buffer) then
      local s = Queue.get(node.buffer, bindex)
      s = s / g_maxAmplitude * 20.0
      node.rbuffer[i] = s
      -- renderstep should be set for each synthnode.
      if node.renderstep == nil then
        bindex = bindex + SynthNode.renderstep
      else
        bindex = bindex + node.renderstep
      end
    else
      node.rbuffer[i] = 0.0
    end
  end
end

