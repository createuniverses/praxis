-- praxis:

function string.at(s, i)
  return string.char(string.byte(s,i))
end

terminalcommand = ""

svrcoro = nil
svrMultiline = false
svrIndents = {}

svrContext = {}
svrContext.print = function(s)
  if s~=nil then
    if type(s) == "table" then s = inspect(s) end
    svrSend(stripnewline(""..s).."\n", sck1)
  else
    svrSend("nil\n", sck1)
  end
  coroutine.yield()
end
setmetatable(svrContext, { __index = _G, __newindex = function (t,n,v) rawset(_G, n,v) end})

function scanf()
  local c = 0
  local s = ""
  while c ~= 10 do
    c = getchar()
    s = s .. string.char(c)
  end
  return s
end

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
  local c,e = load(terminalcommand,
      "terminal", "bt", svrContext)
  if e == nil then
    --print("running:\n" .. terminalcommand)
    --assert(c)()
    terminalcommand = ""
    svrMultiline = false
    
    -- no longer in 5.2
    --setfenv(c,svrContext)

    svrcoro = coroutine.create(c)
  else
    --print("error:\n" .. e)
    local tail = string.sub(e, #e-4, #e)
    --print("tail:\n\"" .. tail.."\"")
    if tail ~= "<eof>" then
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


