
function factorial (n)
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end 
end

print2(factorial(10))
3628800

setBufferName("cps-factorial.lua")

diagfns = {}

function factorial (n)
 local fn = function ()
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end
 end
 table.insert(diagfns, fn)
 return fn()
end

print2(factorial(10))
3628800

print2(#diagfns)
10

print2(diagfns[3]())
40320

720

5040


function factorial (n)
 local fn = function ()
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end
 end
 coroutine.yield(coroutine.create(fn),n)
 --coroutine.yield(n)
 return fn()
end

function factorial (n)
 local fn = function ()
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end
 end
 coroutine.yield(fn,fn())
 return fn()
end

co = coroutine.create(

print2(factorial(10)
co = coroutine.create(function () return factorial(10) end)


print2(coroutine.resume(co))

co1 = co
co = co1
clearTrace()
clearError()

do
  local b,r,n = coroutine.resume(co)
  print(n)
  if type(r) == "thread" then
    co = r
    print("coroutine continuing")
  elseif type(r) == "function" then
    co = coroutine.create(r)
    print("fresh coro")
  else
    print("result = " .. r)
  end
end
  
clearError()

print2

co1 = co

print2(factorial(10))
print2(type(co))
thread
clearError()

boolean
trying to make it all work at once

go slow and easy.
continuation passing style
read sicp
step 1: get lua one shot coroutine to work usably for factorial
step 2: continuation passing style implementation of factorial


thread
print2(coroutine.status(co))
clearError()

Step 1:

function factorial (n)
 local fn = function ()
  if n <= 1 then
    return 1
  else
    return n * factorial(n-1)
  end
 end
 coroutine.yield()
 return fn()
end

co = coroutine.create(function () return factorial(10) end)

do
  while coroutine.status(co) ~= "dead" do
    local d,r = coroutine.resume(co)
    if d then print("true") else print("false") end
    if r ~= nil then
      print(r)
    end
  end
end

function factorial (n)
 local function factorial_cps(n,continuation)
  if n <= 1 then
    return continuation(1)
  else
    return factorial_cps(n-1, function (m) return continuation(n * m) end)
  end
 end
 return factorial_cps(n, function (n) return n end)
end

print2(factorial(10))
3628800

function factorial (n)
 local function factorial_cps(n,continuation)
  if n <= 1 then
    return continuation(1)
  else
    return function ()
      return factorial_cps(n-1, function (m) return continuation(n * m) end)
     end
  end
 end
 return factorial_cps(n, function (n) return n end)
end

fn = factorial(10)

print2(inspect(debug.getinfo(fn)))
{
  currentline = -1,
  func = nil --[[<function 1>]],
  istailcall = false,
  isvararg = false,
  lastlinedefined = 8,
  linedefined = 6,
  namewhat = "",
  nparams = 0,
  nups = 3,
  short_src = '[string "function factorial (n)..."]',
  source = "function factorial (n)\\n local function factorial_cps(n,continuation)\\n  if n <= 1 then\\n    return continuation(1)\\n  else\\n    return function ()\\n      return factorial_cps(n-1, function (m) return continuation(n * m) end)\\n     end\\n  end\\n end\\n return factorial_cps(n, function (n) return n end)\\nend",
  what = "Lua"
}

do
 local di = debug.getinfo(fn)
 --print2(di.nups)
 for i=1,di.nups,1 do
  local name,val = debug.getupvalue(fn,i)
  local t = type(val)
  --print2(type(val))
  if t == "number" then
    print2(name .. ": " .. val)
  else
    print2(name .. ": <" .. type(val) .. ">")
  end
  --print2(type(debug.getupvalue(fn,i)))
  --print2((debug.getupvalue(fn,i)))
 end
end

depth = 0

function factorial (n)
 local function factorial_cps(n,continuation)
  if n <= 1 then
    return continuation(1)
  else
    return function ()
      return factorial_cps(n-1, function (m) return continuation(n * m) end)
     end
  end
 end
 return factorial_cps(n, function (n) return n end)
end

function printlocals(fn)
 local di = debug.getinfo(fn)
 local nups = di.nups
 local nparams = di.nparams
 print2("Params: " .. nparams)
 for i=1,nparams,1 do
  local name,val = debug.getlocal(fn,i)
  local t = type(val)
  if t == "number" then
    print2(name .. ": " .. val)
  else
    print2(name .. ": <" .. type(val) .. ">")
    --print2(name .. ": " .. val)
  end
 end
 print2("Upvals: " .. nups)
 for i=1,nups,1 do
  local name,val = debug.getupvalue(fn,i)
  local t = type(val)
  if t == "number" then
    print2(name .. ": " .. val)
  else
    print2(name .. ": <" .. type(val) .. ">")
  end
  if t == "function" then
    print2(getFunction(val))
    if val == fn then
      --print2("Not examining " .. name .. "...")
      --print2("same as caller")
    else
      --depth = depth + 1
      --if depth < 10 then
      print2("Examining " .. name .. "...")
      printlocals(val)
      --end
      --depth = depth - 1
    end
  end
 end
end

fn = factorial(10)

printlocals(fn)
Params: 0
Upvals: 3
factorial_cps: <function>
 local function factorial_cps(n,continuation)
  if n <= 1 then
    return continuation(1)
  else
    return function ()
      return factorial_cps(n-1, function (m) return continuation(n * m) end)
     end
  end
 end

Examining factorial_cps...
Params: 2
n: <nil>
continuation: <nil>
Upvals: 1
factorial_cps: <function>
 local function factorial_cps(n,continuation)
  if n <= 1 then
    return continuation(1)
  else
    return function ()
      return factorial_cps(n-1, function (m) return continuation(n * m) end)
     end
  end
 end

n: 10
continuation: <function>
 return factorial_cps(n, function (n) return n end)

Examining continuation...
Params: 1
n: <nil>
Upvals: 0



fn = fn() printlocals(fn)
Params: 0
Upvals: 3
factorial_cps: <function>
 local function factorial_cps(n,continuation)
  if n <= 1 then
    return continuation(1)
  else
    return function ()
      return factorial_cps(n-1, function (m) return continuation(n * m) end)
     end
  end
 end

Examining factorial_cps...
Params: 2
n: <nil>
continuation: <nil>
Upvals: 1
factorial_cps: <function>
 local function factorial_cps(n,continuation)
  if n <= 1 then
    return continuation(1)
  else
    return function ()
      return factorial_cps(n-1, function (m) return continuation(n * m) end)
     end
  end
 end

n: 8
continuation: <function>
      return factorial_cps(n-1, function (m) return continuation(n * m) end)

Examining continuation...
Params: 1
m: <nil>
Upvals: 2
continuation: <function>
      return factorial_cps(n-1, function (m) return continuation(n * m) end)

Examining continuation...
Params: 1
m: <nil>
Upvals: 2
continuation: <function>
 return factorial_cps(n, function (n) return n end)

Examining continuation...
Params: 1
n: <nil>
Upvals: 0
n: 10
n: 9

Params: 0
Upvals: 3
factorial_cps: <function>
 local function factorial_cps(n,continuation)
  if n <= 1 then
    return continuation(1)
  else
    return function ()
      return factorial_cps(n-1, function (m) return continuation(n * m) end)
     end
  end
 end

Examining factorial_cps...
Params: 2
n: <nil>
continuation: <nil>
Upvals: 1
factorial_cps: <function>
 local function factorial_cps(n,continuation)
  if n <= 1 then
    return continuation(1)
  else
    return function ()
      return factorial_cps(n-1, function (m) return continuation(n * m) end)
     end
  end
 end

n: 9
continuation: <function>
      return factorial_cps(n-1, function (m) return continuation(n * m) end)

Examining continuation...
Params: 1
m: <nil>
Upvals: 2
continuation: <function>
 return factorial_cps(n, function (n) return n end)

Examining continuation...
Params: 1
n: <nil>
Upvals: 0
n: 10

