
function svrReceive2()
  local s = svrReceive()
  
  if s == "" then return s end
  
  -- discard messages starting with newline
  while string.byte(s,1) == 13 do
    s = svrReceive()
  end
  
  local empty = svrReceive()
  
  return s
end

print_backup = print

function svrRunLua(sck,s)
  print = function(s) svrSend(stripnewline(""..s).."\n", sck) end
  luaCall(s)
  local err = getErrorText()
  if err ~= "" then
    svrSend(err, sck)
    clearError()
  end
  print = print_backup
end

function wordwrap(s, n)
  local s2 = ""
  local c  = n
  s2 = string.sub(s,1,n)
  while c < #s do
    s2 = s2 .. "\n" .. string.sub(s, c+1, c+n)
    c = c + n
  end
  if s2:byte(#s2) ~= 10 then s2 = s2 .. "\n" end
  -- hello
  return s2
end

function svrRunLisp(sck,s)
  if stripspaces(s)~="" then
    svrSend(stripnewline(wordwrap(lisp(s), 60)).."\n", sck)
  end
end

function svrRunForth(sck,s)
  local result = stripnewline(forth(s))
  if result ~= "" then result = result .. "\n" end
  svrSend(result, sck)
  --svrSend(stripnewline(forth(s)).."\n", sck)
end

svrRunCode = svrRunLua

function stripspaces(s)
  local c = string.byte(" ", 1)
  local s2 = s
  
  while string.byte(s2, 1) == c do
    s2 = string.sub(s2,2,#s2)
  end
  
  return s2
end

function stripnewline(s)
  local c1 = string.byte("\n", 1)
  local c2 = string.byte("\r", 1)
  
  local s2 = s
  
  if string.byte(s2, #s2)==c1 then s2 = string.sub(s2,1,#s2-1) end
  if string.byte(s2, #s2)==c2 then s2 = string.sub(s2,1,#s2-1) end
  if string.byte(s2, #s2)==c1 then s2 = string.sub(s2,1,#s2-1) end
  if string.byte(s2, #s2)==c2 then s2 = string.sub(s2,1,#s2-1) end
  
  return s2
end

function svrRunPromptServer(sck)
  local s = svrReceive(sck)
  if s ~= "" then
    --print(s .. " " .. #s)
    s = stripnewline(s)
    --print(s .. " " .. #s)
    if     s == "lua"    then svrRunCode = svrRunLua
    elseif s == "forth"  then svrRunCode = svrRunForth
    elseif s == "lisp"   then svrRunCode = svrRunLisp
    else
      svrRunCode(sck,s)
    end
    svrSend("> ", sck)
  end
end

--svrRunPromptServer = svrRunLuaPromptServer

function svrRunEchoServer(sck)
  -- echo server
  local s = svrReceive(sck)
  if s~="" then
    clearTrace()
    for i=1,#s,1 do
      print(string.byte(s,i))
    end
  end
  svrSend(s, sck)
end

function initServer()
  svrStart()
  coroutine.yield()
  
  repeat
    sck1 = svrAccept()
    coroutine.yield()
  until svrIsValidSocket(sck1)
  
  svrSend("You are the lua prompt terminal\n> ", sck1)
  coroutine.yield()
  
  repeat
    sck2 = svrAccept()
    coroutine.yield()
  until svrIsValidSocket(sck2)
  
  svrSend("You are the echo terminal", sck2)
end

function initServer()
  svrStart()
  print("Server started")
  coroutine.yield()
  
  print("Waiting for terminal client...")
  repeat
    sck1 = svrAccept()
    coroutine.yield()
  until svrIsValidSocket(sck1)
  
  print("Terminal client connected.")

  svrSend("You are the lua prompt terminal\n> ", sck1)
  coroutine.yield()
  
  print("Waiting for echo client...")
  
  repeat
    sck2 = svrAccept()
    coroutine.yield()
  until svrIsValidSocket(sck2)
  
  print("Echo client connected.")
  
  svrSend("You are the echo terminal", sck2)
end

function restartServer()
  initServerRoutine = coroutine.create(initServer)
  coroutine.resume(initServerRoutine)
end

if initServerRoutine == nil then
  restartServer()
end

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





