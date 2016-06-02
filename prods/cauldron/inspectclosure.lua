
function inspectclosure(fn)
 local di = debug.getinfo(fn)

 local nparams = di.nparams
 print2("Params: " .. nparams)
 for i=1,nparams,1 do
  local name = debug.getlocal(fn,i)
  print2(name)
 end

 local nups = di.nups
 print2("Upvals: " .. nups)
 for i=1,nups,1 do
  local name,val = debug.getupvalue(fn,i)
  local t = type(val)
  print2(name .. ": <" .. type(val) .. ">")
  
  if t == "number" then
    print2(" => " .. val)
  end
  
  if t == "table" then
    --print2(inspect(val))
  end

  if t == "function" then
    print2(getFunction(val))
    if val ~= fn then
      print2("Examining " .. name .. "...")
      inspectclosure(val)
    end
  end
 end
end
