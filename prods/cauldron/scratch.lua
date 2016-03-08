-- praxis:
dofile("server2.lua")
clearError()
continue()

-- praxis:
serverfc = 0

Widgets["server"].update = function (o)
  if coroutine.status(initServerRoutine) ~= "dead" then
    coroutine.resume(initServerRoutine)
  else
    svrRunPromptServer(sck1)
    svrRunEchoServer(sck2)
  end
  
  serverfc = serverfc + 1
end

-- praxis:
dofile("errortest.lua")

-- praxis:
clearError()

-- praxis:
setClipboardText(getErrorText())
