-- praxis:

function string.at(s, i)
  return string.char(string.byte(s,i))
end

terminalcommand = ""

svrcoro = nil
svrMultiline = false

function svrRunPromptServer(sck)
  if svrcoro == nil then
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
      if svrMultiline then
        svrSend(">> ", sck)
      end
    end
  else
    --svrSend("Running...", sck)
    local r,msg = coroutine.resume(svrcoro)
    if r==false then
      svrSend(msg.."\n", sck)
      svrSend(debug.traceback(svrcoro).."\n", sck)
      svrcoro = nil
      svrSend("> ", sck)
    elseif coroutine.status(svrcoro) ~= "suspended" then
      svrcoro = nil
      svrSend("> ", sck)
    end
  end
end

function svrRunLua(sck,s)
  terminalcommand = terminalcommand .. s .. "\n"
  local c,e = loadstring(terminalcommand)
  if e == nil then
    local context = {}
    setmetatable(context, { __index = _G })
    context.print = function(s)
      if s~=nil then
        svrSend(stripnewline(""..s).."\n", sck)
      else
        svrSend("nil\n", sck)
      end
      coroutine.yield()
    end
    --print("running:\n" .. terminalcommand)
    --assert(c)()
    terminalcommand = ""
    svrMultiline = false
    setfenv(c,context)
    svrcoro = coroutine.create(c)
  else
    local tail = string.sub(e, #e-6, #e)
    if tail ~= "'<eof>'" then
      --print("error for " .. terminalcommand .. "...\n")
      terminalcommand = ""
      svrSend(e.."\n", sck)
      --print("\ntail was \""..tail.."\"\n")
      svrMultiline = false
    else
      svrMultiline = true
      --print("multiline continuing for:\n" .. terminalcommand)
    end
  end
end

svrRunCode = svrRunLua

iocommand = ""

function svrRunIo(sck,s)
  iolang("cli_line := \"\"\"" .. s .. "\"\"\"")
  local reply,trace = iolang(s)
  do
  local result = stripnewline(reply)
  if result ~= "" then result = "==> " .. result .. "\n" end
  svrSend(result, sck)
  end
  do
  local result = stripnewline(trace)
  if result ~= "" then result = result .. "\n" end
  svrSend(result, sck)
  end
end

