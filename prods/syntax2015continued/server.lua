
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

-- praxis:

iolang("doFile(\"iocli.io\")")

iocommand = ""

function svrRunIo(sck,s)
  iocommand = iocommand .. s .. "\n"
  local reply,trace = iolang("doLine(\"\"\" " .. iocommand .. " \"\"\")")
  if reply == "Ready" then
    --iolang("context set_(getSlot(\"lastResult\")")
    local result = stripnewline(iolang("getSlot(\"lastResult\")"))
    if result ~= "nil" or trace == "" then
      result = result .. "\n"
      svrSend(result, sck)
    end
    iocommand = ""
  end
  if reply == "Error" then
    local result = stripnewline(iolang("lastError"))
    result = result .. "\n"
    svrSend(result, sck)
    iocommand = ""
  end
  if reply == "Incomplete" then
    result = "..." .. "\n"
    svrSend(result, sck)
  end
  
  trace = stripnewline(trace)
  if trace ~= "" then
    trace = trace .. "\n"
    svrSend(trace, sck)
  end
end

svrRunCode = svrRunIo

-- praxis:

function svrRunIo(sck,s)
  local reply,trace = iolang(s)
  do
    local result = stripnewline(reply)
    result = "==> " .. result .. "\n"
    svrSend(result, sck)
  end
  do
    local result = stripnewline(trace)
    if result ~= "" then
      result = "Error: " .. result .. "\n"
      svrSend(result, sck)
    end
  end
end

svrRunCode = svrRunIo

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
    elseif s == "io"     then svrRunCode = svrRunIo
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

Widgets["server"] = WidgetLib2.newSimple("server")

Widgets["server"].update = function (o)
  if coroutine.status(initServerRoutine) ~= "dead" then
    coroutine.resume(initServerRoutine)
  else
    svrRunPromptServer(sck1)
    svrRunEchoServer(sck2)
  end
end

