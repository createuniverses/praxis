-- praxis:

function string.at(s, i)
  return string.char(string.byte(s,i))
end

terminalcommand = ""

function svrRunLua(sck,s)
  print = function(s)
            if s~=nil then
              svrSend(stripnewline(""..s).."\n", sck)
            else
              svrSend("nil\n", sck)
            end
          end
  terminalcommand = terminalcommand .. s .. "\n"
  local c,e = loadstring(terminalcommand)
  if e == nil then
    --print("running:\n" .. terminalcommand)
    --assert(c)()
    terminalcommand = ""
    local r,msg = pcall(c)
    if r==false then
      svrSend(msg.."\n", sck)
    end
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
  print = print_backup
end

svrRunCode = svrRunLua

