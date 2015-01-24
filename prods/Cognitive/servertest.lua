svrStart()
svrAccept()

setBufferName("servertest.lua")

print2(getFunction("update"))

function svrClear()
  svrSend(string.char(27).."[2J")
end

function svrUpdate()
  local s = svrReceive()
  if s ~= "" then
    luaCall(s)
    local err = getErrorText()
    if err ~= "" then
      svrSend(err)
      clearError()
    end
    -- svrSend("Ready.\n")
    --svrSend("\n> ")
    svrSend("> ")
  end
end

function svrEchoUpdate()
  -- echo server
  local s = svrReceive()
  if s~="" then
    clearTrace()
    for i=1,#s,1 do
      print(string.byte(s,i))
    end
  end
  svrSend(s)
end


function update()
  WidgetLib.callAll("update")
  
  if true then
    local s = svrReceive()
    if s ~= "" then
      luaCall(s)
      local err = getErrorText()
      if err ~= "" then
        svrSend(err)
        clearError()
      end
      -- svrSend("Ready.\n")
      --svrSend("\n> ")
      svrSend("> ")
    end
  end
  
  
  -- echo server
  if false then
    local s = svrReceive()
    if s~="" then
      clearTrace()
      for i=1,#s,1 do
        print(string.byte(s,i))
      end
    end
    svrSend(s)
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

function update()
  WidgetLib.callAll("update")
  
  if true then
    local s = svrReceive()
    if s ~= "" then
      luaCall(s)
      local err = getErrorText()
      if err ~= "" then
        svrSend(err)
        clearError()
      end
      -- svrSend("Ready.\n")
      --svrSend("\n> ")
      svrSend("> ")
    end
  end
  
  
  -- echo server
  if false then
    local s = svrReceive()
    if s~="" then
      clearTrace()
      for i=1,#s,1 do
        print(string.byte(s,i))
      end
    end
    svrSend(s)
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


