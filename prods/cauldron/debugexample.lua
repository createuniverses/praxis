function factorial(n)
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end
end

--print2(factorial(10))
--3628800

function printdebuginfo(...)
  printf("\n")
  local args = {...}
  --printf("Args:\n" .. inspect(args) .. "\n")
  local d = debug.getinfo(2)
  --printf(d.source)
  local linetext = selectLines(d.source, args[2], args[2])
  printf(linetext)
  --printf(inspect(d))
  printf("Upvalues:\n")
  for i=1,d.nups,1 do
    local name, value = debug.getupvalue(d.func, i)
    printf(name .. " = " .. type(value) .. "\n")
  end

  printf("Locals:\n")
  local i = 1
  while true do
    local name, value = debug.getlocal(2,i)
    if not name then break end
    local t = type(value)
    if t == "number" or t == "string" then
      printf(name .. " = " .. value .. "\n")
    else
      printf(name .. " is a " .. t .. "\n")
    end
    if t == "function" then
      --printf(getFunction(value))
    end
    if t == "table" then
      --printf(inspect(value))
    end
    i = i + 1
  end
end

do
  debug.sethook(printdebuginfo, "l")
  printf(factorial(5) .. "\n")
  debug.sethook()
  turnOnDebugHook()
end


