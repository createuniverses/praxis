-- praxis:

function string.at(s, i)
  return string.char(string.byte(s,i))
end

terminalcommand = ""

svrcoro = nil

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
      -- check if multiline here
      svrSend("1> ", sck)
    end
  else
    coroutine.resume(svrcoro)
    if coroutine.status(svrcoro) ~= "suspended" then
      svrcoro = nil
      svrSend("2> ", sck)
    end
  end
end

function svrRunLua(sck,s)
  local context = {}
  setmetatable(context, { __index = _G })
  context.print = function(s)
            if s~=nil then
              svrSend(stripnewline(""..s).."\n", sck)
            else
              svrSend("nil\n", sck)
            end
            --coroutine.yield() -- 
          end
  terminalcommand = terminalcommand .. s .. "\n"
  local c,e = loadstring(terminalcommand)
  if e == nil then
    --print("running:\n" .. terminalcommand)
    --assert(c)()
    terminalcommand = ""
    setfenv(c,context)
    local corofn = function ()
      local r,msg = pcall(c)
      if r==false then
        svrSend(msg.."\n", sck)
      end
    end
    svrcoro = coroutine.create(corofn)
    coroutine.resume(svrcoro)
  else
    local tail = string.sub(e, #e-6, #e)
    if tail ~= "'<eof>'" then
      --print("error for " .. terminalcommand .. "...\n")
      terminalcommand = ""
      svrSend(e.."\n", sck)
      --print("\ntail was \""..tail.."\"\n")
    else
      --print("multiline continuing for:\n" .. terminalcommand)
    end
  end
  --print = print_backup
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

